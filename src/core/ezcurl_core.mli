(** Core signatures and implementation *)

(** Configuration for the client. *)
module Config : sig
  type t

  val default : t
  val verbose : bool -> t -> t
  val authmethod : Curl.curlAuth list -> t -> t
  val max_redirects : int -> t -> t
  val follow_location : bool -> t -> t
  val username : string -> t -> t
  val password : string -> t -> t
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

type t = {
  curl: Curl.t;
  set_opts: Curl.t -> unit;
}
(** A client, i.e. a cURL instance. The wrapping record has been present since
    0.3 *)

val make :
  ?set_opts:(Curl.t -> unit) ->
  ?cookiejar_file:string ->
  ?enable_session_cookies:bool ->
  unit ->
  t
(** Create a new client.
    @param set_opts called before returning the client, to set options
    @param cookiejar_file
      if provided, tell curl to use the given file path to store/load cookies
      (since 0.3)
    @param enable_session_cookies
      if provided, enable cookie handling in curl so it store/load cookies
      (since 0.3) *)

val delete : t -> unit
(** Delete the client. It cannot be used anymore. *)

val with_client : ?set_opts:(Curl.t -> unit) -> (t -> 'a) -> 'a
(** Make a temporary client, call the function with it, then cleanup. *)

val set_no_signal : bool -> unit
(** Set no_signal default value for each new client instance. Default is [true].
    See [CURLOPT_NOSIGNAL].
    @since 0.3 *)

(** Cookie handling.

    @since 0.3 *)
module Cookies : sig
  val flush_cookiejar : t -> unit
  (** If [cookiejar_file] was provided in {!make}, this flushes the current set
      of cookies to the provided file.
      @since 0.3 *)

  val reload_cookiejar : t -> unit
  (** If [cookiejar_file] was provided in {!make}, this reloads cookies from the
      provided file.
      @since 0.3 *)

  val get_cookies : t -> string list
  (** Get cookie list (in netscape format) *)

  val set_cookies : t -> string list -> unit
  (** Set cookie list (in netscape format) *)

  val transfer : t -> t -> unit
  (** [transfer c1 c2] copies cookies in [c1] into [c2] *)
end

(* TODO: duphandle is deprecated, how do we iterate on options?
   val copy : t -> t
*)

type response_info = {
  ri_response_time: float;
      (** Total time (in seconds) for the request/response pair. See
          {!Curl.get_totaltime}. *)
  ri_redirect_count: int;
      (** Number of redirects cURL followed. See {!Curl.get_redirectcount}. *)
}
(** Metadata about a response from the server. *)

val pp_response_info : Format.formatter -> response_info -> unit
val string_of_response_info : response_info -> string

type 'body response = {
  code: int;
      (** Response code. See
          https://developer.mozilla.org/en-US/docs/Web/HTTP/Status *)
  headers: (string * string) list;  (** Response headers *)
  body: 'body;  (** Response body, or [""] *)
  info: response_info;  (** Information about the response *)
}
(** Response for a given request. *)

val pp_response_with :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a response -> unit

val pp_response : Format.formatter -> string response -> unit
val string_of_response : string response -> string

(** The
    {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods} HTTP method}
    to use *)
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
  | HTTP_CUSTOM of string  (** Custom HTTP method (e.g., for WebDAV) *)

val pp_meth : Format.formatter -> meth -> unit
val string_of_meth : meth -> string

(** {2 Underlying IO Monad} *)
module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val fail : exn -> 'a t
  val perform : Curl.t -> Curl.curlCode t
end

(** {2 Main Signature} *)
module type S = sig
  type 'a io

  val http :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?content:[ `String of string | `Write of bytes -> int -> int ] ->
    ?headers:(string * string) list ->
    url:string ->
    meth:meth ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** General purpose HTTP call via cURL.
      @param url the URL to query
      @param meth which method to use (see {!meth})
      @param tries how many times to retry in case of [CURLE_AGAIN] code
      @param client a client to reuse (instead of allocating a new one)
      @param range
        an optional
        {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests}
         byte range} to fetch (either to get large pages by chunks, or to resume
        an interrupted download).
      @param config configuration to set
      @param content
        the content to send as the query's body, either a [`String s] to write a
        single string, or [`Write f] where [f] is a callback that is called on a
        buffer [b] with len [n] (as in [f b n]) and returns how many bytes it
        wrote in the buffer [b] starting at index [0] (at most [n] bytes). It
        must return [0] when the content is entirely written, and not before.
      @param headers headers of the query *)

  (** Push-based stream of bytes
      @since 0.3 *)
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
    url:string ->
    meth:meth ->
    write_into:#input_stream ->
    unit ->
    (unit response, Curl.curlCode * string) result io
  (** HTTP call via cURL, with a streaming response body. The body is given to
      [write_into] by chunks, then [write_into#on_close ()] is called and the
      response is returned.
      @since 0.3 *)

  val get :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?range:string ->
    ?headers:(string * string) list ->
    url:string ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:GET] See {!http} for more info. *)

  val put :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string * string) list ->
    url:string ->
    content:[ `String of string | `Write of bytes -> int -> int ] ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:PUT] See {!http} for more info. *)

  val post :
    ?tries:int ->
    ?client:t ->
    ?config:Config.t ->
    ?headers:(string * string) list ->
    ?content:[ `String of string | `Write of bytes -> int -> int ] ->
    params:Curl.curlHTTPPost list ->
    url:string ->
    unit ->
    (string response, Curl.curlCode * string) result io
  (** Shortcut for [http ~meth:(POST params)] See {!http} for more info. *)
end

module Make (IO : IO) : S with type 'a io = 'a IO.t
