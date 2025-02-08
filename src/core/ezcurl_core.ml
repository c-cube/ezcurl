let opt_iter ~f = function
  | None -> ()
  | Some x -> f x

module Config = struct
  type t = {
    verbose: bool;
    authmethod: Curl.curlAuth list option;
    max_redirects: int;
    follow_location: bool;
    username: string option;
    password: string option;
    user_agent: string option;
  }

  let default : t =
    {
      verbose = false;
      max_redirects = 50;
      follow_location = true;
      authmethod = None;
      username = None;
      password = None;
      user_agent = Some "curl";
    }

  let password x self = { self with password = Some x }
  let username x self = { self with username = Some x }
  let verbose x self = { self with verbose = x }
  let follow_location x self = { self with follow_location = x }
  let max_redirects x self = { self with max_redirects = max 1 x }
  let authmethod x self = { self with authmethod = Some x }

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

  let pp out (self : t) =
    let {
      verbose;
      authmethod;
      max_redirects;
      follow_location;
      username;
      password;
      user_agent;
    } =
      self
    in
    Format.fprintf out
      "{@[verbose=%B;@ max_redirects=%d;@ follow_location=%B;@ username=%s;@ \
       password=%s;@ authmethod=%s;@ user_agent=%s@]}"
      verbose max_redirects follow_location (str_of_str_opt username)
      (str_of_str_opt password)
      (match authmethod with
      | None -> "none"
      | Some l -> List.map string_of_authmethod l |> String.concat ",")
      (str_of_str_opt user_agent)

  let to_string s = Format.asprintf "%a" pp s
end

type t = { curl: Curl.t } [@@unboxed]
type client = t

let _top_mutex = Mutex.create ()

let _with_mutex f =
  Mutex.lock _top_mutex;
  match f () with
  | res ->
    Mutex.unlock _top_mutex;
    res
  | exception e ->
    Mutex.unlock _top_mutex;
    raise e

let _init =
  let initialized = ref false in
  fun () ->
    _with_mutex @@ fun () ->
    if not !initialized then (
      initialized := true;
      Curl.global_init Curl.CURLINIT_GLOBALALL;
      at_exit Curl.global_cleanup
    )

let make ?(set_opts = fun _ -> ()) ?cookiejar_file
    ?(enable_session_cookies = false) () : t =
  _init ();
  let curl = Curl.init () in
  Gc.finalise Curl.cleanup curl;
  opt_iter cookiejar_file ~f:(fun file ->
      Curl.set_cookiejar curl file;
      Curl.set_cookiefile curl file);
  if enable_session_cookies then Curl.set_cookiefile curl "";
  set_opts curl;
  { curl }

let delete (self : t) = Curl.cleanup self.curl
let _cfg_no_signal = ref false (* default: 0 *)
let _get_no_signal () : bool = _with_mutex @@ fun () -> !_cfg_no_signal
let set_no_signal v = _with_mutex @@ fun () -> _cfg_no_signal := v

module Cookies = struct
  let reload_cookiejar (self : t) : unit =
    Curl.set_cookielist self.curl "RELOAD"

  let flush_cookiejar (self : t) : unit = Curl.set_cookielist self.curl "FLUSH"
  let get_cookies self = Curl.get_cookielist self.curl

  let set_cookies self (l : string list) =
    List.iter (Curl.set_cookielist self.curl) l

  let transfer c1 c2 = set_cookies c2 @@ get_cookies c1
end

(* set options *)
let _apply_config (self : t) (config : Config.t) : unit =
  let {
    Config.verbose;
    max_redirects;
    follow_location;
    authmethod;
    username;
    password;
    user_agent;
  } =
    config
  in
  Curl.set_verbose self.curl verbose;
  Curl.set_maxredirs self.curl max_redirects;
  Curl.set_followlocation self.curl follow_location;
  opt_iter user_agent ~f:(fun user_agent ->
      Curl.set_useragent self.curl user_agent);
  opt_iter authmethod ~f:(Curl.set_httpauth self.curl);
  opt_iter username ~f:(Curl.set_username self.curl);
  opt_iter password ~f:(Curl.set_password self.curl);
  ()

let _eq_case a b =
  let low = String.lowercase_ascii in
  String.equal (low a) (low b)

let _add_header_nodup (h : string * string) (headers : _ list ref) : unit =
  let sq = List.to_seq !headers in
  let k, v = h in
  if not (List.exists (fun (tk,tv) -> _eq_case k tk && _eq_case v tv) !headers) then
    headers := h :: !headers;

let _contains_resp_headers (h : string) (headers : string list) : bool =
  List.exists (_eq_case h) headers

let _set_headers (self : t) (headers : _ list) : unit =
  let headers = List.map (fun (k, v) -> k ^ ": " ^ v) headers in
  Curl.set_httpheader self.curl headers;
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
  let { ri_response_time; ri_redirect_count } = r in
  Format.fprintf out "{@[response_time=%.3fs;@ redirect_count=%d@]}"
    ri_response_time ri_redirect_count

let string_of_response_info s = Format.asprintf "%a" pp_response_info s

type 'body response = {
  code: int;
  headers: (string * string) list;
  body: 'body;
  info: response_info;
}

let pp_response_with ppbody out r =
  let pp_header out (s1, s2) = Format.fprintf out "@[<2>%s:@ %s@]" s1 s2 in
  let pp_headers out l =
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_header) l
  in
  let { code; body; headers; info } = r in
  Format.fprintf out "{@[code=%d;@ headers=@[%a@];@ info=%a;@ body=@[%a@]@]}"
    code pp_headers headers pp_response_info info ppbody body

let pp_response = pp_response_with Format.pp_print_text
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

type sse_frame = {
  event: string option;
  id: string option;
  data: string option;
  retry: int option;
  empties: string list; (* Lines without a ':' *)
}

type sse_state =
  | Frame of sse_frame
  | End_of_stream

type sse_callback = sse_state -> bool

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
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
    ?content:[ `String of string | `Write of bytes -> int -> int ] ->
    ?headers:(string * string) list ->
    ?callback:[ `Sse_event of sse_callback ] ->
    url:string ->
    meth:meth ->
    unit ->
    (string response, Curl.curlCode * string) result io
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

  (** Push-stream of bytes
      @since NEXT_RELEASE *)
  class type input_stream = object
    method on_close : unit -> unit
    method on_input : bytes -> int -> int -> unit
  end

  val http_stream :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?content:[ `String of string | `Write of bytes -> int -> int ] ->
    ?headers:(string * string) list ->
    ?callback:[ `Sse_event of sse_callback ] ->
    url:string ->
    meth:meth ->
    write_into:#input_stream ->
    unit ->
    (unit response, Curl.curlCode * string) result io
  (** HTTP call via cURL, with a streaming response body.
      @since NEXT_RELEASE *)

  val get :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?headers:(string * string) list ->
    ?callback:[ `Sse_event of sse_callback ] ->
    url:string ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:GET]
      See {!http} for more info.
  *)

  val put :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string * string) list ->
    ?callback:[ `Sse_event of sse_callback ] ->
    url:string ->
    content:[ `String of string | `Write of bytes -> int -> int ] ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:PUT]
      See {!http} for more info.
  *)

  val post :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string * string) list ->
    ?content:[ `String of string | `Write of bytes -> int -> int ] ->
    ?callback:[ `Sse_event of sse_callback ] ->
    params:Curl.curlHTTPPost list ->
    url:string ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:(POST params)]
      See {!http} for more info.
  *)
end

exception Parse_error of Curl.curlCode * string

let mk_res (self : t) headers body : (_ response, _) result =
  let split_colon s =
    match String.index s ':' with
    | exception Not_found ->
      raise
        (Parse_error (Curl.CURLE_CONV_FAILED, "header line without a ':': " ^ s))
    | i ->
      ( String.sub s 0 i,
        String.trim (String.sub s (i + 1) (String.length s - i - 1)) )
  in
  try
    let code = Curl.get_httpcode self.curl in
    let headers =
      match headers with
      | [] -> []
      | _ :: tl ->
        List.map split_colon tl (* first one is "http1.1 NNN <descr>" *)
    in
    let info =
      {
        ri_redirect_count = Curl.get_redirectcount self.curl;
        ri_response_time = Curl.get_totaltime self.curl;
      }
    in
    Ok { headers; code; body; info }
  with Parse_error (e, msg) -> Error (e, Curl.strerror e ^ ": " ^ msg)

let sse_frame_with_event sse_f v =
  {
    (!sse_f) with
    event = Some v;
    }

let sse_frame_with_id sse_f v =
  {
    event = !sse_f.event;
    id = Some v;
    data = !sse_f.data;
    retry = !sse_f.retry;
    empties = !sse_f.empties;
  }

let sse_frame_append_data sse_f v =
  let data =
    match !sse_f.data with
    | None -> Some v
    | Some vv -> Some (vv ^ "\n" ^ v)
  in
  {
    event = !sse_f.event;
    id = !sse_f.id;
    data;
    retry = !sse_f.retry;
    empties = !sse_f.empties;
  }

let sse_frame_with_retry sse_f v =
  let retry = int_of_string_opt v in
  {
    event = !sse_f.event;
    id = !sse_f.id;
    data = !sse_f.data;
    retry;
    empties = !sse_f.empties;
  }

let sse_frame_append_empties sse_f v =
  let empties =
    match !sse_f.empties with
    | [] -> [ v ]
    | vv -> v :: vv
  in
  {
    event = !sse_f.event;
    id = !sse_f.id;
    data = !sse_f.data;
    retry = !sse_f.retry;
    empties;
  }

let sse_process_pair k v sse_f =
  match k with
  | "event" -> sse_f := sse_frame_with_event sse_f v
  | "id" -> sse_f := sse_frame_with_id sse_f v
  | "data" -> sse_f := sse_frame_append_data sse_f v
  | "retry" -> sse_f := sse_frame_with_retry sse_f v
  | "" -> () (* The field is ignored *)
  | _ -> sse_f := sse_frame_append_empties sse_f k

let sse_split_line s =
  let l = String.length s in
  let sq = String.to_seq s in
  match Seq.find_index (fun c -> c = ':') sq with
  | None -> s :: []
  | Some p -> [ String.sub s 0 p; String.sub s (p + 1) (l - p - 1) ]

let sse_parse_line line sse_f =
  match sse_split_line line with
  | [ k; v ] ->
    let k = String.trim k in
    let v = String.trim v in
    sse_process_pair k v sse_f
  | [ k ] ->
    let k = String.trim k in
    sse_process_pair k "" sse_f
  | _ ->
    ();
    ()

let sse_extract_next_line body =
  let len = Buffer.length body in
  let bf_seq = Buffer.to_seq body in
  (* Search for some complete line *)
  match Seq.find_index (fun c -> c = '\n') bf_seq with
  (* Then no complete line available for now *)
  | None -> None
  (* Oh nice a complete line found *)
  | Some pivot ->
    (* Extract line except ending LF *)
    let bf_line = Bytes.create pivot in
    Buffer.blit body 0 bf_line 0 pivot;
    let line = String.trim (Bytes.to_string bf_line) in
    (* Now shift left the remaining after LF *)
    let pivot = pivot + 1 in
    let rem = len - pivot in
    let bf_after = Bytes.create rem in
    if rem > 0 then Buffer.blit body pivot bf_after 0 rem;
    Buffer.reset body;
    Buffer.add_bytes body bf_after;
    (* Here the line finally *)
    Some line

let sse_parse_lines body sse_f =
  let rec loop body sse_f =
    match sse_extract_next_line body with
    | None -> false (* Nothing for now *)
    | Some line ->
      (match line with
      (* Ready to send event *)
      | "" -> true
      (* Try next line *)
      | line ->
        sse_parse_line line sse_f;
        loop body sse_f)
  in
  loop body sse_f

let sse_handle_post_write callback body sse_f =
  match sse_f with
  | None -> true (* Stream can continue (No SSE content) *)
  | Some _sse_f ->
    (match callback with
    | None -> true (* Stream can continue (No SSE callback) *)
    | Some (`Sse_event sse_cb) ->
      let sse_cb_clean sse_cb _sse_f =
        (* Send callback to user *)
        let r = sse_cb (Frame !_sse_f) in
        (* And reset internal event data *)
        _sse_f :=
          { event = None; id = None; data = None; retry = None; empties = [] };
        r
      in
      let rec loop body sse_f =
        match sse_parse_lines body sse_f with
        (* Stream can continue *)
        | false -> true
        (* Must dispatch event now*)
        | true ->
          (match sse_cb_clean sse_cb sse_f with
          (* Stream must close now *)
          | false -> false
          (* Else continue parse *)
          | true -> loop body sse_f)
      in
      loop body _sse_f)

let sse_handle_post_finish callback body sse_f =
  match sse_f with
  | None -> ()
  | Some _ ->
    (match callback with
    | None -> ()
    | Some (`Sse_event sse_cb) ->
      let _ = sse_handle_post_write callback body sse_f in
      let _ = sse_cb End_of_stream in
      ())

module Make (IO : IO) : S with type 'a io = 'a IO.t = struct
  open IO

  type 'a io = 'a IO.t

  let content_read_fun_ content =
    match content with
    | `String s ->
      let n = ref 0 in
      fun i ->
        let len = min i (String.length s - !n) in
        let r = String.sub s !n len in
        n := !n + len;
        r
    | `Write f ->
      let buf = Bytes.create 1024 in
      fun i ->
        let len = min i (Bytes.length buf) in
        let n = f buf len in
        Bytes.sub_string buf i n

  let content_size_ = function
    | `String s -> Some (String.length s)
    | `Write _ -> None

  class type input_stream = object
    method on_close : unit -> unit
    method on_input : bytes -> int -> int -> unit
  end

  type http_state_ = {
    client: client;
    do_cleanup: bool;
    mutable resp_headers: string list;
    mutable resp_headers_done: bool;
    mutable sse_frame: sse_frame ref option;
  }

  let http_setup_ ?client ?(config = Config.default) ?range ?content
      ?(headers = []) ?callback ~url ~meth () : http_state_ =
    let headers = ref headers in
    let do_cleanup, self =
      match client with
      | None -> true, make ()
      | Some c -> false, c
    in
    _apply_config self config;
    opt_iter range ~f:(fun s -> Curl.set_range self.curl s);

    (* TODO: ability to make content a stream with a `read` function *)
    opt_iter content ~f:(fun content ->
        Curl.set_readfunction self.curl (content_read_fun_ content);
        (* also set size if known *)
        match content_size_ content, meth with
        | None, _ ->
          headers :=
            ("expect", "") :: ("transfer-encoding", "chunked") :: !headers
        | Some size, POST _ -> Curl.set_postfieldsize self.curl size
        | Some size, _ -> Curl.set_infilesize self.curl size);

    (* Add more pre-determined request headers depend of feature *)
    (match callback with
    | None -> ()
    | Some (`Sse_event _) ->
      _add_header_nodup ("Cache-control", "no-cache") headers;
      _add_header_nodup ("Accept", "text/event-stream") headers;
      ());

    (* local state *)
    let st =
      {
        do_cleanup;
        client = self;
        resp_headers = [];
        resp_headers_done = false;
        sse_frame = None;
      }
    in

    (* once we get "\r\n" header line *)
    Curl.set_url self.curl url;
    (match meth with
    | POST [] when content <> None -> Curl.set_post self.curl true
    | POST l -> Curl.set_httppost self.curl l
    | GET -> Curl.set_httpget self.curl true
    | PUT ->
      Curl.set_customrequest self.curl "PUT";
      Curl.set_upload self.curl true
    | DELETE -> Curl.set_customrequest self.curl "DELETE"
    | HEAD -> Curl.set_customrequest self.curl "HEAD"
    | CONNECT -> Curl.set_customrequest self.curl "CONNECT"
    | OPTIONS -> Curl.set_customrequest self.curl "OPTIONS"
    | TRACE -> Curl.set_customrequest self.curl "TRACE"
    | PATCH -> Curl.set_customrequest self.curl "PATCH");

    _set_headers self !headers;
    Curl.set_headerfunction self.curl (fun s0 ->
        let s = String.trim s0 in
        (* Printf.printf "got header %S\n%!" s0; *)
        if s0 = "\r\n" then (
          st.resp_headers_done <- true ;
          (* Validate headers for user callback *)
          match callback with
          | None -> ()
          | Some (`Sse_event _) ->
            if
              _contains_resp_headers "Content-type: text/event-stream"
                st.resp_headers
            then
              st.sse_frame <-
                Some
                  (ref
                     {
                       event = None;
                       id = None;
                       data = None;
                       retry = None;
                       empties = [];
                     })
        ) else (
          (* redirection: drop previous headers *)
          if st.resp_headers_done then (
            st.resp_headers_done <- false;
            st.resp_headers <- []
          );

          st.resp_headers <- s :: st.resp_headers
        );
        String.length s0);

    st

  let http ?(tries = 1) ?client ?config ?range ?content ?headers ?callback
      ~url ~meth () :
      (string response, _) result io =
    (* at least one attempt *)
    let tries = max tries 1 in
    let st =
      http_setup_ ?client ?config ?range ?content ?headers ?callback ~url ~meth ()
    in

    let body = Buffer.create 64 in
    Curl.set_writefunction st.client.curl (fun s ->
        Buffer.add_string body s;
        match sse_handle_post_write callback body st.sse_frame with
        | true -> String.length s (* Continue write *)
        | false -> 0xFFFFFFFE
        (* Stop stream, not-forked libcurl has no CURL_WRITE_FUNC_ABORT *));

    let rec loop i =
      IO.perform st.client.curl >>= function
      | Curl.CURLE_OK ->
        let _ = sse_handle_post_finish callback body st.sse_frame in
        let r =
          mk_res st.client (List.rev st.resp_headers) (Buffer.contents body)
        in
        if st.do_cleanup then Curl.cleanup st.client.curl;
        return r
      | Curl.CURLE_AGAIN when i > 1 -> loop (i - 1) (* try again *)
      | c ->
        let _ = sse_handle_post_finish callback body st.sse_frame in
        if st.do_cleanup then Curl.cleanup st.client.curl;
        return (Error (c, Curl.strerror c))
    in
    loop tries

  let http_stream ?(tries = 1) ?client ?config ?range ?content ?headers
      ?callback ~url ~meth ~(write_into : #input_stream) () : (unit response, _) result io =
    let tries = max tries 1 in
    let st =
      http_setup_ ?client ?config ?range ?content ?headers ?callback ~url ~meth ()
    in

    let body_sse_ =
      match callback with
      | None -> None
      | Some (`Sse_event _) -> Some (Buffer.create 64) in
    Curl.set_writefunction st.client.curl (fun s ->
        let n = String.length s in
        write_into#on_input (Bytes.unsafe_of_string s) 0 n;
        match body_sse_ with
        | None -> n
        | Some body ->
            Buffer.add_string body s;
            (match sse_handle_post_write callback body st.sse_frame with
             | true -> n (* Continue write *)
             | false -> 0xFFFFFFFE
             (* Stop stream, not-forked libcurl has no CURL_WRITE_FUNC_ABORT *))
        );

    let rec loop i =
      IO.perform st.client.curl >>= function
      | Curl.CURLE_OK ->
        (match body_sse_ with
         | None -> ()
         | Some body -> let _ = sse_handle_post_finish callback body st.sse_frame in ());
        let r = mk_res st.client (List.rev st.resp_headers) () in
        write_into#on_close ();
        if st.do_cleanup then Curl.cleanup st.client.curl;
        return r
      | Curl.CURLE_AGAIN when i > 1 -> loop (i - 1) (* try again *)
      | c ->
        (match body_sse_ with
        | None -> ()
        | Some body -> let _ = sse_handle_post_finish callback body st.sse_frame in ());
        write_into#on_close ();
        if st.do_cleanup then Curl.cleanup st.client.curl;
        return (Error (c, Curl.strerror c))
    in
    loop tries

  let get ?tries ?client ?config ?range ?headers ?callback ~url () : _ result io =
    http ?tries ?client ?config ?range ?headers ?callback ~url ~meth:GET ()

  let post ?tries ?client ?config ?headers ?content ?callback ~params ~url () :
      _ result io =
    http ?tries ?client ?config ?headers ?content ?callback ~url ~meth:(POST params) ()

  let put ?tries ?client ?config ?headers ?callback ~url ~content () : _ result io =
    http ?tries ?client ?config ?headers ?callback ~url ~content ~meth:PUT ()
end
