module H = Tiny_httpd

(** Start server, return its port and a thread *)
let start_server () : int * Thread.t =
  let port = ref (-1) in
  let cond = Condition.create () in
  let mutex = Mutex.create () in

  let server = H.create ~masksigpipe:true ~addr:"127.0.0.1" ~port:0 () in
  H.add_route_handler server
    H.Route.(exact "test" @/ string @/ return)
    (fun str _req ->
      H.Response.make_string ~code:200 @@ Ok (Printf.sprintf "hello %s" str));
  let th =
    Thread.create
      (H.run_exn ~after_init:(fun () ->
           port := H.port server;
           Condition.broadcast cond))
      server
  in

  (* wait for server to start *)
  while !port < 0 do
    Mutex.lock mutex;
    Condition.wait cond mutex;
    Mutex.unlock mutex
  done;
  !port, th

let test1 ~port () =
  let name = "jeanjacques" in
  let url = Printf.sprintf "http://127.0.0.1:%d/test/%s" port name in
  match Ezcurl.get ~url () with
  | Error (code, msg) ->
    Format.eprintf "curl error: code `%s` (%s)@." (Curl.strerror code) msg
  | Ok res ->
    Format.printf "get: OK@.code=%d,body=```@.%s@.```@." res.code res.body

let test2 ~port () =
  let name = "reineclaude" in
  let url = Printf.sprintf "http://127.0.0.1:%d/test/%s" port name in
  let buf = Buffer.create 32 in
  match
    Ezcurl.http_stream ~meth:GET ~url
      ~write_into:
        (object
           method on_input bs i len = Buffer.add_subbytes buf bs i len
           method on_close () = ()
        end)
      ()
  with
  | Error (code, msg) ->
    Format.eprintf "curl error: code `%s` (%s)@." (Curl.strerror code) msg
  | Ok res ->
    let new_body = Buffer.contents buf in
    Format.printf "streaming get: OK@.code=%d, body=```@.%s@.```@." res.code
      new_body

let () =
  let port, _th = start_server () in
  test1 ~port ();
  test2 ~port ();
  exit 0
