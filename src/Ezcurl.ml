type t = Curl.t

let _init = lazy (
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  at_exit Curl.global_cleanup;
)

let make ?(set_opts=fun _ -> ()) () : t =
  Lazy.force _init;
  let c = Curl.init () in
  Gc.finalise Curl.cleanup c;
  set_opts c;
  c

let delete = Curl.cleanup

let with_client ?set_opts f =
  let c = make ?set_opts () in
  try
    let x = f c in
    delete c;
    x
  with e ->
    delete c;
    raise e

type response_info = {
  ri_response_time: float;
  ri_redirect_count: int;
}

type response = {
  code: int;
  headers: (string * string) list;
  body: string;
  info: response_info;
}

exception Parse_error of Curl.curlCode * string

let mk_res (self:t) headers body : (response,_) result =
  let split_colon s =
    match String.index s ':' with
    | exception Not_found ->
      raise (Parse_error (Curl.CURLE_CONV_FAILED, "header line without a ':': " ^ s))
    | i ->
      String.sub s 0 i,
      String.trim (String.sub s (i+1) (String.length s-i-1))
  in
  try
    let code = Curl.get_httpcode self in
    let headers =
      match headers with
      | [] -> []
      | _ :: tl -> List.map split_colon tl (* first one is "http1.1 NNN <descr>" *)
    in
    let info = {
      ri_redirect_count=Curl.get_redirectcount self;
      ri_response_time=Curl.get_totaltime self;
    } in
    Ok {headers; code; body; info}
  with Parse_error (e, msg) ->
    Error (e, Curl.strerror e ^ ": " ^ msg)

type meth =
  | GET
  | POST of Curl.curlHTTPPost list
  | PUT

let string_of_meth = function
  | GET -> "GET"
  | POST _ -> "POST"
  | PUT -> "PUT"

let opt_map ~f = function None -> None | Some x -> Some (f x)
let opt_iter ~f = function None -> () | Some x -> f x

let http ?(verbose=false) ?(tries=1)
    ?client ?authmethod ?(max_redirects=50) ?(follow_location=false)
    ?username ?password ?(headers=[])
    ~url ~meth () : _ result =
  let do_cleanup, self = match client with
    | None -> true, make()
    | Some c ->
      Curl.reset c;
      false, c
  in
  (* local state *)
  let tries = max tries 1 in (* at least one attempt *)
  let body = ref "" in  
  let resp_headers = ref [] in
  let resp_headers_done = ref false in (* once we get "\r\n" header line *)
  (* start setting options *)
  Curl.set_url self url;
  Curl.set_verbose self verbose;
  Curl.set_maxredirs self max_redirects;
  Curl.set_followlocation self follow_location;
  Curl.set_header self false;
  begin match meth with
    | POST l -> Curl.set_httppost self l;
    | GET -> Curl.set_httpget self true;
    | PUT -> Curl.set_put self true;
  end;
  opt_iter authmethod ~f:(Curl.set_httpauth self);
  opt_iter username ~f:(Curl.set_username self);
  opt_iter password ~f:(Curl.set_password self);
  let headers =
    List.map (fun (k,v) -> k ^ ": " ^ v ^ "\r\n") headers
  in
  Curl.set_httpheader self headers;
  Curl.set_headerfunction self
    (fun s0 ->
       let s = String.trim s0 in
       (* Printf.printf "got header %S\n%!" s0; *)
       if s0 = "\r\n" then (
         resp_headers_done := true;
       ) else (
         (* redirection: drop previous headers *)
         if !resp_headers_done then (
           resp_headers_done := false;
           resp_headers := [];
         );
         
         resp_headers := s :: !resp_headers; 
       );
       String.length s0);
  Curl.set_writefunction self
    (fun s ->
       body := (if !body = "" then s else !body ^ s);
       String.length s);
  let rec loop i =
    match Curl.perform self with
    | () ->
      let r = mk_res self (List.rev !resp_headers) !body in
      if do_cleanup then Curl.cleanup self;
      r
    | exception Curl.CurlException (Curl.CURLE_AGAIN, _, _) when i > 1 ->
      (* another try *)
      loop (i-1)
    | exception Curl.CurlException (c, _, _) ->
      if do_cleanup then Curl.cleanup self;
      Error (c, Curl.strerror c)
  in
  loop tries

let get ?verbose ?tries ?client ?authmethod ?max_redirects
    ?follow_location
    ?username ?password ?headers ~url () : _ result =
  http ?verbose ?tries ?client ?authmethod ?max_redirects
    ?follow_location
    ?username ?password ?headers ~url ~meth:GET ()

(* TODO
let post ?verbose ?tries ?client ?auth ?username ?password ~url () : _ result =
  call ?verbose ?tries ?client ?auth ?username ?password ~url ~meth:GET ()
   *)
