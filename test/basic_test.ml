let body = ref ""

let url =
  "https://raw.githubusercontent.com/c-cube/ezcurl/refs/heads/main/.ocamlformat"

let () =
  match Ezcurl.get ~url () with
  | Error (code, msg) ->
    Format.eprintf "curl error: code `%s` (%s)@." (Curl.strerror code) msg
  | Ok res ->
    body := res.body;
    Format.printf "get: OK@.body=```@.%s@.```@." !body

let () =
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
  | Ok _res ->
    let new_body = Buffer.contents buf in
    Format.printf "streaming get: OK@.body=```@.%s@.```@." new_body;
    Format.printf "same buf? %b@." (new_body = !body)
