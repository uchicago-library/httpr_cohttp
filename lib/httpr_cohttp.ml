(* the redirect-chasing code is adapted from: *)
(* https://github.com/mirage/ocaml-cohttp/blob/main/README.md#dealing-with-redirects *)

module Response = Httpr_intf.Response

let empty_headers = Http.Header.init ()

let rec http_get_and_follow ~max_redirects ~req_headers uri
    =
  let open Lwt.Syntax in
  let* ans =
    Cohttp_lwt_unix.Client.get ~headers:req_headers uri
  in
  follow_redirect ~max_redirects ~req_headers uri ans

and follow_redirect ~max_redirects ~req_headers request_uri
    (response, body) =
  let open Lwt.Syntax in
  let status = Http.Response.status response in
  let* () =
    if status <> `OK
    then Cohttp_lwt.Body.drain_body body
    else Lwt.return_unit
  in
  match (status, max_redirects) with
  | _, 0 -> Lwt.return (response, body)
  | `OK, _ -> Lwt.return (response, body)
  | `Permanent_redirect, _ | `Moved_permanently, _ ->
    handle_redirect ~permanent:true ~max_redirects
      ~req_headers request_uri response
  | `Found, _ | `Temporary_redirect, _ ->
    handle_redirect ~permanent:false ~max_redirects
      ~req_headers request_uri response
  | `Not_found, _ | `Gone, _ -> failwith "Not found"
  | status, _ ->
    Printf.ksprintf failwith "Unhandled status: %s"
      (Cohttp.Code.string_of_status status)

and handle_redirect ~permanent ~max_redirects ~req_headers
    request_uri response =
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
    http_get_and_follow ~req_headers uri
      ~max_redirects:(max_redirects - 1)

let cohttp_to_httpr ~max_redirects ~req_headers uri =
  let open Cohttp in
  let open Lwt.Syntax in
  let* resp, body_stream =
    http_get_and_follow ~max_redirects ~req_headers uri
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
let long_timeout = Int32.(max_int |> to_int)

let wrap_with_timeout ?(timeout = long_timeout)
    result_promise =
  let noun = if timeout = 1 then "second" else "seconds" in
  let msg = Printf.sprintf "%d %s" timeout noun in
  let open Lwt.Infix in
  Lwt.pick
    [ result_promise;
      ( Lwt_unix.sleep (float_of_int timeout)
      >|= fun () -> Error msg )
    ]

(* let* promise = f *)
(* let* promise = result_promise in *)
(* let compute ~timeout ~f = *)
(*   [ *)
(*     (f () >|= fun v -> `Done v) *)
(*   ; (Lwt_unix.sleep timeout >|= fun () -> `Timeout) *)
(*   ] *)
(* in *)

(* let open Lwt_result.Syntax in *)
(* let timeout = *)
(*   let* () = Lwt_unix.sleep (float_of_int timeout) in *)
(*   let exception Timeout of string in *)
(* let noun = *)
(*   if timeout = 1 then "second" else "seconds" *)
(* in *)
(* let msg = Printf.sprintf "%d %s" timeout noun in *)
(*   Lwt.fail (Timeout msg) *)
(* in *)
(* Lwt.pick [ timeout; promised ] *)

let prep_headers lst =
  let each_header str =
    match Prelude.String.split ~sep:":" str with
    | [ k; v ] -> Ok Stdlib.String.(trim k, trim v)
    | _ -> Error "invalid headers"
  in
  let open Etude.Result.Make (String) in
  oks (List.map each_header lst) |> Http.Header.of_list

let get_promise_no_timeout_exn ?(redirects = -1)
    ?(headers = []) uri =
  let req_headers = prep_headers headers in
  cohttp_to_httpr ~max_redirects:redirects ~req_headers uri

let get_promise_no_timeout ?(verbose = false)
    ?(redirects = -1) ?(headers = []) uri =
  let _ = verbose in
  Lwt_result.catch (fun () ->
      get_promise_no_timeout_exn ~redirects ~headers uri )
  |> Lwt_result.map_error Printexc.to_string

let get_promise_blob_no_timeout_exn ?(redirects = -1)
    ?(headers = []) (blob, uri) =
  let open Lwt.Syntax in
  let req_headers = prep_headers headers in
  let+ resp =
    cohttp_to_httpr ~max_redirects:redirects ~req_headers
      uri
  in
  (blob, resp)

let get_promise_blob_no_timeout ?(verbose = false)
    ?(redirects = -1) ?(headers = []) pair =
  let _ = verbose in
  Lwt_result.catch (fun () ->
      get_promise_blob_no_timeout_exn ~redirects ~headers
        pair )
  |> Lwt_result.map_error Printexc.to_string

let get_promise ?(timeout = 0) ?(verbose = false)
    ?(redirects = -1) ?(headers = []) uri =
  let _ = verbose in
  let timeout' =
    if timeout <= 0 then long_timeout else timeout
  in
  let promise =
    get_promise_no_timeout ~verbose ~redirects ~headers uri
  in
  wrap_with_timeout ~timeout:timeout' promise

let execute promise =
  match Lwt_main.run promise with
  | exception e -> Error (Printexc.to_string e)
  | success -> Ok success

let get ?(timeout = 0) ?(verbose = false) ?(redirects = -1)
    ?(headers = []) uri =
  let _ = verbose in
  execute
    (get_promise ~timeout ~verbose ~redirects ~headers uri)
  |> Result.join

(* let parallel_get f ?(timeout = 0) lst = *)
(*   assert false *)

(* let gets ?(timeout = 0) ?(verbose = false) ?(redirects = -1) *)
(*     ?(headers = []) uris = *)
(*   let promise = *)
(*     get_promise_no_timeout ~verbose ~redirects ~headers *)
(*   in *)
(*   parallel_get promise ~timeout uris *)

(* let gets_keyed ?(timeout = 0) ?(verbose = false) *)
(*     ?(redirects = -1) ?(headers = []) pairs = *)
(*   let promise = *)
(*     get_promise_blob_no_timeout ~verbose ~redirects ~headers *)
(*   in *)
(*   parallel_get promise ~timeout pairs *)
