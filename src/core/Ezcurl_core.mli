
(** {1 Core signatures and implementation} *)

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

type t = Curl.t

val make :
  ?set_opts:(t -> unit) ->
  unit -> t

val delete : t -> unit

val with_client :
  ?set_opts:(t -> unit) ->
  (t -> 'a) -> 'a
(** Make a temporary client, call the function with it, then cleanup *)

(* TODO: duphandle is deprecated, how do we iterate on options?
val copy : t -> t
   *)

type response_info = {
  ri_response_time: float;
  ri_redirect_count: int;
}

val pp_response_info : Format.formatter -> response_info -> unit
val string_of_response_info : response_info -> string

type response = {
  code: int;
  headers: (string * string) list;
  body: string;
  info: response_info;
}

val pp_response : Format.formatter -> response -> unit
val string_of_response : response -> string

(** The {{: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods} HTTP method}
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

val pp_meth : Format.formatter -> meth -> unit
val string_of_meth : meth -> string

(** {2 Underlying IO Monad} *)
module type IO = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
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

module Make(IO : IO) : S with type 'a io = 'a IO.t
