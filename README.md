# EZCurl

A simple wrapper around OCurl, for easy tasks around http.

## Installation

- for the synchronous library: `opam install ezcurl`
- for the lwt-baed library: `opam install ezcurl-lwt` (depends on `ezcurl`)

## Usage

### Synchronous API

The library lives in a module `Ezcurl`, which wraps `Curl.t` with functions
such as `get` that combine many different low level API calls into one.
It also follows redirections by default, and returns a `Ezcurl.response`
object that contains the body, headers, and error code.

```ocaml
# #require "ezcurl";;
# let url = "https://curl.haxx.se/";;
val url : string = "https://curl.haxx.se/"
# let res = Ezcurl.get ~url ();;
val res : (Ezcurl_core.response, Curl.curlCode * string) result =
  Ok
   {Ezcurl_core.code = 200;
    headers =
...
# let content = match res with Ok c -> c | Error (_,s) -> failwith s;;
val content : Ezcurl_core.response =
  {Ezcurl_core.code = 200;
   headers =
...

# content.code;;
- : int = 200
```


### Lwt API

Using `ezcurl-lwt`, a module `Ezcurl_lwt` becomes available, with
functions that are similar to the ones in `Ezcurl` but are non blocking.
This makes it easy to run several queries in parallel:

```ocaml
# #require "ezcurl-lwt";;
# let urls = [
  "https://en.wikipedia.org/wiki/CURL";
  "https://en.wikipedia.org/wiki/OCaml";
  "https://curl.haxx.se/";
  ];;
val urls : string list =
  ["https://en.wikipedia.org/wiki/CURL";
   "https://en.wikipedia.org/wiki/OCaml"; "https://curl.haxx.se/"]

# open Lwt.Infix;;
# let codes =
    List.map (fun url -> Ezcurl_lwt.get ~url ()) urls
    |> Lwt_list.map_p
    (fun fut ->
      fut >>= function
      | Ok r -> Lwt.return r.Ezcurl_lwt.code
      | Error e -> Lwt.fail (Failure "oh no"))
  ;;
val codes : int list Ezcurl_lwt.io = <abstr>

# codes;;
- : int list = [200; 200; 200]
```
