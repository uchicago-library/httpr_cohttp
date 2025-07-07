(* the redirect-chasing code is adapted from: *)
(* https://github.com/mirage/ocaml-cohttp/blob/main/README.md#dealing-with-redirects *)

let rec http_get_and_follow ~max_redirects uri =
  let open Lwt.Syntax in
  let* ans = Cohttp_lwt_unix.Client.get uri in
  follow_redirect ~max_redirects uri ans

and follow_redirect ~max_redirects request_uri
    (response, body) =
  let open Lwt.Syntax in
  let status = Http.Response.status response in
  let* () =
    if status <> `OK
    then Cohttp_lwt.Body.drain_body body
    else Lwt.return_unit
  in
  match status with
  | `OK -> Lwt.return (response, body)
  | `Permanent_redirect | `Moved_permanently ->
    handle_redirect ~permanent:true ~max_redirects
      request_uri response
  | `Found | `Temporary_redirect ->
    handle_redirect ~permanent:false ~max_redirects
      request_uri response
  | `Not_found | `Gone -> failwith "Not found"
  | status ->
    Printf.ksprintf failwith "Unhandled status: %s"
      (Cohttp.Code.string_of_status status)

and handle_redirect ~permanent ~max_redirects request_uri
    response =
  if max_redirects <= 0
  then failwith "Too many redirects"
  else
    let headers = Http.Response.headers response in
    let location = Http.Header.get headers "location" in
    match location with
    | None -> failwith "Redirection without Location header"
    | Some url ->
      let open Lwt.Syntax in
      let uri = Uri.of_string url in
      let* () =
        if permanent
        then
          Logs_lwt.warn (fun m ->
              m "Permanent redirection from %s to %s"
                (Uri.to_string request_uri)
                url )
        else Lwt.return_unit
      in
      http_get_and_follow uri
        ~max_redirects:(max_redirects - 1)

let cohttp_to_httpr ~max_redirects uri =
  let open Cohttp in
  let open Lwt.Syntax in
  let* resp, body_stream =
    http_get_and_follow ~max_redirects uri
  in
  let headers =
    let header_alist = resp.headers |> Header.to_list in
    let each_pair (k, v) = k ^ ": " ^ v in
    List.map each_pair header_alist
  in
  let status, reason =
    let code = Code.code_of_status resp.status in
    Code.(code, code |> reason_phrase_of_code)
  in
  let ctype =
    match Header.get_media_type resp.headers with
    | Some ctype -> ctype
    | None -> "application/octet-stream"
  in
  let* body = Cohttp_lwt.Body.to_string body_stream in
  Lwt.return
    { Httpr_intf.Response.uri;
      status;
      reason;
      headers;
      ctype;
      body
    }

let ssl_init _ = ()

(* timeout code adapted from: *)
(* https://discuss.ocaml.org/t/timeout-cohttprequests/660 *)

let wrap_with_timeout ?(timeout = max_int) promised =
  let open Lwt.Syntax in
  let timeout =
    let* () = Lwt_unix.sleep (float_of_int timeout) in
    let exception Timeout of string in
    let msg = Printf.sprintf "%d seconds" timeout in
    Lwt.fail (Timeout msg)
  in
  Lwt.pick [ timeout; promised ]

let get ?(timeout = 0) ?(verbose = false) ?(redirects = -1)
    ?(headers = []) uri =
  (* TODO: finesse redirects *)
  (* TODO: finesse zero case of timeout *)
  (* TODO: finesse negative case of timeout *)
  (* TODO: do headers *)
  (* TODO: do verbose *)
  let promised =
    cohttp_to_httpr ~max_redirects:redirects uri
  in
  match
    Lwt_main.run (wrap_with_timeout ~timeout promised)
  with
  | exception Failure s -> Error s
  | exception e -> Error (Printexc.to_string e)
  | success -> Ok success
