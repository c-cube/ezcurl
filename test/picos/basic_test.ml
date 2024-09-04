let () =
  Picos_mux_fifo.run @@ fun () ->
  match
    Ezcurl_picos.get
      ~url:
        "https://archive.softwareheritage.org/api/1/content/sha1_git:7bdf38d4468c114206c9b6ebd9cf1176e085d346/"
      ()
  with
  | Error (code, msg) ->
    Format.eprintf "curl error: code `%s` (%s)@." (Curl.strerror code) msg
  | Ok _response -> Format.printf "OK@."
