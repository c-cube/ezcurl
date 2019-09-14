
(** {1 Synchronous API} *)

include Ezcurl_core

include Ezcurl_core.Make(struct
    type 'a t = 'a
    let return x = x
    let (>>=) x f = f x
    let (>|=) x f = f x
    let fail e = raise e
    let perform c =
      try Curl.perform c; Curl.CURLE_OK
      with Curl.CurlException (c, _, _) -> c
  end)

