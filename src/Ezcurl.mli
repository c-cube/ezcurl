type t

val make :
  ?set_opts:(Curl.t -> unit) ->
  unit -> t

val delete : t -> unit

val with_client :
  ?set_opts:(Curl.t -> unit) ->
  (t -> 'a) -> 'a
(** Make a temporary client, call the function with it, then cleanup *)

(* TODO: duphandle is deprecated, how do we iterate on options?
val copy : t -> t
   *)

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

type meth =
  | GET
  | POST of Curl.curlHTTPPost list
  | PUT

val string_of_meth : meth -> string

val http :
  ?verbose:bool ->
  ?tries:int ->
  ?client:t ->
  ?authmethod:Curl.curlAuth list ->
  ?max_redirects:int ->
  ?follow_location:bool ->
  ?username:string ->
  ?password:string ->
  ?headers:(string*string) list ->
  url:string ->
  meth:meth ->
  unit ->
  (response, Curl.curlCode * string) result

val get :
  ?verbose:bool ->
  ?tries:int ->
  ?client:t ->
  ?authmethod:Curl.curlAuth list ->
  ?max_redirects:int ->
  ?follow_location:bool ->
  ?username:string ->
  ?password:string ->
  ?headers:(string*string) list ->
  url:string ->
  unit ->
  (response, Curl.curlCode * string) result

