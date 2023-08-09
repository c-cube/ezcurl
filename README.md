# EZCurl [![build](https://github.com/c-cube/ezcurl/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/ezcurl/actions/workflows/main.yml)

A simple wrapper around OCurl, for easy tasks around http.

**project goals**

- be as simple to use as possible.
- be as reliable as possible (work is done by cURL and the ocurl bindings anyway).
- stable API with few dependencies, so that user code compiles without breakage
  for a long time.

## Installation

- for the synchronous library: `opam install ezcurl`
- for the lwt-based library: `opam install ezcurl-lwt` (depends on `ezcurl`)

## Usage

A small web crawler can be found in `examples/argiope`. It's very incomplete
and naive but demonstrates basic usage of `Ezcurl_lwt.get`.

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
...
# let content = match res with Ok c -> c | Error (_,s) -> failwith s;;
val content : Ezcurl_core.response =
...

# content.Ezcurl.code;;
- : int = 200
```

It is also possible to create a client with `Ezcurl.make()` and re-use
it across calls with the optional parameter `client`.

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
...
# codes;;
- : int list = [200; 200; 200]
```
