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
      ~on_write:(fun bs ~length -> Buffer.add_subbytes buf bs 0 length)
      ()
  with
  | Error (code, msg) ->
    Format.eprintf "curl error: code `%s` (%s)@." (Curl.strerror code) msg
  | Ok _res ->
    let new_body = Buffer.contents buf in
    Format.printf "streaming get: OK@.body=```@.%s@.```@." new_body;
    Format.printf "same buf? %b@." (new_body = !body)
