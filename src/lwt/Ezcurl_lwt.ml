
include Ezcurl_core

include Make(struct
    include Lwt
    let perform = Curl_lwt.perform
end)
