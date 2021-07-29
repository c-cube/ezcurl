
let opt_iter ~f = function None -> () | Some x -> f x

module Config = struct
  type t = {
    verbose: bool;
    authmethod: Curl.curlAuth list option;
    max_redirects: int;
    follow_location: bool;
    username: string option;
    password: string option;
  }

  let default : t = {
    verbose=false;
    max_redirects = 50;
    follow_location=true;
    authmethod=None;
    username=None;
    password=None;
  }

  let password x self = {self with password=Some x}
  let username x self = {self with username=Some x}
  let verbose x self = { self with verbose=x}
  let follow_location x self = {self with follow_location=x}
  let max_redirects x self = {self with max_redirects=max 1 x}
  let authmethod x self = {self with authmethod=Some x}

  let string_of_authmethod = function
    | Curl.CURLAUTH_ANY -> "any"
    | Curl.CURLAUTH_BASIC -> "basic"
    | Curl.CURLAUTH_DIGEST -> "digest"
    | Curl.CURLAUTH_GSSNEGOTIATE -> "gss_negotiate"
    | Curl.CURLAUTH_NTLM -> "ntlm"
    | Curl.CURLAUTH_ANYSAFE -> "any_safe"

  let str_of_str_opt = function
    | None -> "<none>"
    | Some s -> s

  let pp out (self:t) =
    let {
      verbose; authmethod; max_redirects; follow_location;
      username; password;
    } = self in
    Format.fprintf out
      "{@[verbose=%B;@ max_redirects=%d;@ follow_location=%B;@ \
       username=%s;@ password=%s;@ authmethod=%s@]}"
      verbose max_redirects follow_location
      (str_of_str_opt username) (str_of_str_opt password)
      (match authmethod with
       | None -> "none"
       | Some l -> List.map string_of_authmethod l |> String.concat ",")

  let to_string s = Format.asprintf "%a" pp s
end

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

(* set options *)
let _apply_config (self:t) (config:Config.t) : unit =
  let {
    Config.verbose; max_redirects; follow_location; authmethod;
    username; password;
  } = config in
  Curl.set_verbose self verbose;
  Curl.set_maxredirs self max_redirects;
  Curl.set_followlocation self follow_location;
  opt_iter authmethod ~f:(Curl.set_httpauth self);
  opt_iter username ~f:(Curl.set_username self);
  opt_iter password ~f:(Curl.set_password self);
  ()

let _set_headers (self:t) (headers: _ list) : unit =
  let headers =
    List.map (fun (k,v) -> k ^ ": " ^ v ^ "\r\n") headers
  in
  Curl.set_httpheader self headers;
  ()

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

let pp_response_info out r =
  let {ri_response_time; ri_redirect_count} = r in
  Format.fprintf out "{@[response_time=%.3fs;@ redirect_count=%d@]}"
    ri_response_time ri_redirect_count

let string_of_response_info s = Format.asprintf "%a" pp_response_info s

type response = {
  code: int;
  headers: (string * string) list;
  body: string;
  info: response_info;
}

let pp_response out r =
  let pp_header out (s1,s2) =
    Format.fprintf out "@[<2>%s:@ %s@]" s1 s2
  in
  let pp_headers out l =
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_header) l
  in
  let {code; body; headers; info; } = r in
  Format.fprintf out "{@[code=%d;@ headers=@[%a@];@ info=%a;@ body=@[%a@]@]}"
    code pp_headers headers pp_response_info info
    Format.pp_print_text body


let string_of_response s = Format.asprintf "%a" pp_response s

type meth =
  | GET
  | POST of Curl.curlHTTPPost list
  | PUT
  | DELETE
  | HEAD
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

let string_of_meth = function
  | GET -> "GET"
  | POST _ -> "POST"
  | PUT -> "PUT"
  | DELETE -> "DELETE"
  | HEAD -> "HEAD"
  | CONNECT -> "CONNECT"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | PATCH -> "PATCH"

let pp_meth out m = Format.pp_print_string out (string_of_meth m)

module type IO = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val fail : exn -> 'a t
  val perform : Curl.t -> Curl.curlCode t
end

module type S = sig
  type 'a io

  val http :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?content:[`String of string | `Write of (bytes -> int -> int)] ->
    ?headers:(string*string) list ->
    url:string ->
    meth:meth ->
    unit ->
    (response, Curl.curlCode * string) result io
  (** General purpose HTTP call via cURL.
      @param url the URL to query
      @param meth which method to use (see {!meth})
      @param tries how many times to retry in case of [CURLE_AGAIN] code
      @param client a client to reuse (instead of allocating a new one)
      @param range an optional
      {{: https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests} byte range}
      to fetch (either to get large pages
        by chunks, or to resume an interrupted download).
      @param config configuration to set
      @param content the content to send as the query's body, either
        a [`String s] to write a single string, or [`Write f]
        where [f] is a callback that is called on a buffer [b] with len [n]
        (as in [f b n]) and returns how many bytes it wrote in the buffer
        [b] starting at index [0] (at most [n] bytes).
        It must return [0] when the content is entirely written, and not
        before.
      @param headers headers of the query
  *)

  val get :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?headers:(string*string) list ->
    url:string ->
    unit ->
    (response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:GET]
      See {!http} for more info.
  *)

  val put :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string*string) list ->
    url:string ->
    content:[`String of string | `Write of (bytes -> int -> int)] ->
    unit ->
    (response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:PUT]
      See {!http} for more info.
  *)

  val post :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string*string) list ->
    ?content:[`String of string | `Write of (bytes -> int -> int)] ->
    params:Curl.curlHTTPPost list ->
    url:string ->
    unit ->
    (response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:(POST params)]
      See {!http} for more info.
  *)
end

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

module Make(IO : IO)
  : S with type 'a io = 'a IO.t
= struct
  open IO

  type 'a io = 'a IO.t

  let content_read_fun_ content =
    match content with
    | `String s ->
      let n = ref 0 in
      (fun i ->
         let len = min i (String.length s - !n) in
         let r = String.sub s !n len in
         n := !n + len;
         r)
    | `Write f ->
      let buf = Bytes.create 1024 in
      (fun i ->
         let len = min i (Bytes.length buf) in
         let n = f buf len in
         Bytes.sub_string buf i n)

  let http
      ?(tries=1) ?client ?(config=Config.default) ?range ?content ?(headers=[]) ~url ~meth ()
    : _ result io =
    let do_cleanup, self = match client with
      | None -> true, make()
      | Some c ->
        Curl.reset c;
        false, c
    in
    _apply_config self config;
    opt_iter range ~f:(fun s -> Curl.set_range self s);
    (* TODO: ability to make content a stream with a `read` function *)
    opt_iter content
      ~f:(fun content ->
          Curl.set_readfunction self (content_read_fun_ content));
    (* local state *)
    let tries = max tries 1 in (* at least one attempt *)
    let body = Buffer.create 64 in
    let resp_headers = ref [] in
    let resp_headers_done = ref false in (* once we get "\r\n" header line *)
    Curl.set_url self url;
    begin match meth with
      | POST l -> Curl.set_httppost self l;
      | GET -> Curl.set_httpget self true;
      | PUT -> Curl.set_put self true;
      | DELETE -> Curl.set_customrequest self "DELETE";
      | HEAD -> Curl.set_customrequest self "HEAD"
      | CONNECT -> Curl.set_customrequest self "CONNECT"
      | OPTIONS -> Curl.set_customrequest self "OPTIONS"
      | TRACE -> Curl.set_customrequest self "TRACE"
      | PATCH -> Curl.set_customrequest self "PATCH"
    end;
    _set_headers self headers;
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
         Buffer.add_string body s;
         String.length s);
    let rec loop i =
      IO.perform self >>= function
      | Curl.CURLE_OK ->
        let r = mk_res self (List.rev !resp_headers) (Buffer.contents body) in
        if do_cleanup then Curl.cleanup self;
        return r
      | Curl.CURLE_AGAIN when i > 1 ->
        loop (i-1) (* try again *)
      | c ->
        if do_cleanup then Curl.cleanup self;
        return (Error (c, Curl.strerror c))
    in
    loop tries

  let get ?tries ?client ?config ?range ?headers ~url () : _ result io =
    http ?tries ?client ?config ?range ?headers  ~url ~meth:GET ()

  let post ?tries ?client ?config ?headers ?content ~params ~url () : _ result io =
    http ?tries ?client ?config ?headers ?content ~url ~meth:(POST params) ()

  let put ?tries ?client ?config ?headers ~url ~content () : _ result io =
    http ?tries ?client ?config ?headers  ~url ~content ~meth:PUT ()
end
