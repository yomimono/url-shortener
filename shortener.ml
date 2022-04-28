open Lwt.Infix
open Lwt.Syntax

module Webapp
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RW) = struct
  open Httpaf

  let reserved = [ "/uptime"; "/new"; "/status"; "/"; "/favicon.ico" ]

  let not_found =
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.create ~headers `Not_found, None

  let ise =
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.create ~headers `Internal_server_error, None

  let bad_request =
    let headers = Headers.of_list [ "connection", "close" ] in
    Response.create ~headers `Bad_request, None

  let slash reqd =
    let form =
{html|<!doctype html>
<html>
<head></head>
<body>
<form method="post" enctype="multipart/form-data" action="/new">
  <label>Short name: 
    <input type="text" required="true" id="short" name="short_name" />
  </label>
  <label> URL:
    <input type="text" required="true" id="target" name="url"/>
  </label>
    <button type="submit">Submit</button>
</form>
</body>
</html>|html}
    in
    let headers = Headers.of_list
      [ "content-length", string_of_int (String.length form)
      ; "content-type", "text/html; charset=utf-8" ] in
    Response.create ~headers `OK, Some form

  let uptime start_time =
    let span = Ptime.Span.sub (Ptime.Span.v @@ Pclock.now_d_ps ()) (Ptime.to_span start_time) in
    let contents = Fmt.str "%a (since %a)" Ptime.Span.pp span (Ptime.pp_human ()) start_time in
    let headers = Headers.of_list
      [ "content-type", "text/plain"
      ; "content-length", string_of_int (String.length contents) ] in
    Response.create ~headers `OK, Some contents

  let get_reserved start_time path reqd =
    if String.equal path "/uptime" then uptime start_time
    else if String.equal path "/favicon.ico" then not_found
    else if String.equal path "/" then slash reqd
    else not_found

  let get_from_database kv path =
    KV.get kv @@ Mirage_kv.Key.v path >>= function
    | Error (`Not_found k) ->
      Lwt.return not_found
    | Error e ->
      Logs.err (fun f -> f "error %a fetching a key from the database" KV.pp_error e);
      Lwt.return ise
    | Ok data ->
      let headers = Headers.of_list
        [ "location", data
        ; "content-length", "0" ] in
      Lwt.return (Response.create ~headers `Temporary_redirect, None)

  let validate_uri ~hostname s =
    let uri = Uri.of_string s |> Uri.canonicalize in
    match Uri.scheme uri with
    | None -> None
    | Some scheme ->
      Logs.debug (fun f -> f "uri seems to have a scheme: %S" scheme);
      (* we support only http and https *)
      if not (String.equal scheme "http" || String.equal scheme "https") then None
      else begin
        (* hostname shouldn't be our own hostname *)
        Logs.debug (fun f -> f "uri scheme was valid");
        match Uri.host uri with
        | None -> None
        | Some h when String.equal h hostname -> None
        | Some _ ->
          Logs.debug (fun f -> f "hostname isn't our own hostname");
          (* please don't share your usernames and passwords with me,
           * even if you like me a lot *)
          match Uri.user uri, Uri.password uri with
          | None, None -> Some uri
          | _, _ ->
            Logs.debug (fun f -> f "refusing to save HTTP basic auth");
            None
      end

  let maybe_set kv hostname path url =
    Logs.debug (fun f -> f "valid-looking POST request received; checking to see whether we have it in the db already");
    let path = Uri.pct_encode path in
    (* arbitrarily, shortcuts cannot be longer than 128 characters *)
    if String.length path > 128 then Lwt.return bad_request
    else begin
      KV.exists kv @@ Mirage_kv.Key.v path >>= function
      | Error e ->
        Logs.err (fun f -> f "error %a trying to post a url" KV.pp_error e);
        Lwt.return ise
      | Ok (Some _) ->
        let contents = "There's already a URL set there. Try choosing another." in
        let headers = Headers.of_list
          [ "content-type", "text/plain"
          ; "content-length", string_of_int (String.length contents) ] in
        Lwt.return (Response.create ~headers `Conflict, Some contents)
      | Ok None ->
        match validate_uri ~hostname url with
        | None ->
          Logs.debug (fun f -> f "url failed validation test");
          Lwt.return bad_request
        | Some url ->
          KV.set kv (Mirage_kv.Key.v path) (Uri.to_string url) >>= function
          | Error e ->
            Logs.err (fun m -> m "error setting key: %a" KV.pp_write_error e);
            Lwt.return ise
          | Ok () ->
            Logs.debug (fun f -> f "set a new key");
            let contents = "Success! Your shortcut has been created." in
            let headers = Headers.of_list
              [ "content-type", "text/plain"
              ; "content-length", string_of_int (String.length contents) ] in
            Lwt.return (Response.create ~headers `Created, Some contents)
    end

  let stream_of_body body =
    let stream, push = Lwt_stream.create () in
    let rec on_eof () = push None
    and on_read buf ~off ~len =
      push (Some (Bigstringaf.substring buf ~off ~len)) ;
      Body.schedule_read body ~on_eof ~on_read in
    Body.schedule_read body ~on_eof ~on_read ;
    stream

  let identify header =
    let open Multipart_form in
    let ( >>= ) = Option.bind in
    let ( >>| ) x f = Option.map f x in
    Header.content_disposition header
    >>= Content_disposition.name
    >>| String.lowercase_ascii
    >>= function
    | "url" -> Some `Url
    | "short_name" -> Some `Short_name
    | _ -> None

  let post kv hostname reqd request =
    let headers = request.Request.headers in
    match Headers.get headers "content-type" with
    | None -> Lwt.return bad_request
    | Some str ->
      match Multipart_form.Content_type.of_string (str ^ "\r\n") with
      | Error (`Msg _err) -> Lwt.return bad_request
      | Ok content_type ->
        let body = Reqd.request_body reqd in
        let stream = stream_of_body body in
        let `Parse th, stream = Multipart_form_lwt.stream ~identify
          stream content_type in
        th >>= fun result ->
        Body.close_reader body ;
        ( match result with
        | Error _ -> Lwt.return ise
        | Ok _tree ->
          Lwt_stream.to_list stream
          >>= Lwt_list.filter_map_p (fun (id, headers, stream) ->
            Lwt_stream.to_list stream >|= String.concat "" >>= fun contents ->
            match id with
            | None -> Lwt.return_none
            | Some `Short_name -> Lwt.return_some (`Short_name, contents)
            | Some `Url -> Lwt.return_some (`Url, contents)) >>= fun bindings ->
          match List.assoc_opt `Short_name bindings,
                List.assoc_opt `Url bindings with
          | Some path, Some url when not (List.mem path reserved) ->
            maybe_set kv hostname path url
          | _ -> Lwt.return bad_request )

  let reply (kv : KV.t) hostname start_time (_ipaddr, _port) reqd =
    let res () =
      Lwt.catch begin fun () ->
        let request = Reqd.request reqd in
        let path =
          Uri.of_string request.Request.target
          |> Uri.canonicalize
          |> Uri.path in
        let* response, body = match request.Request.meth with
          | `GET when List.mem path reserved ->
            Lwt.return (get_reserved start_time path reqd)
          | `GET -> get_from_database kv path
          | `POST -> post kv hostname reqd request
          | _ ->
            let headers = Headers.of_list [ "connection", "close" ] in
            Lwt.return (Response.create ~headers `Method_not_allowed, None) in
        let body = Option.value ~default:"" body in
        Reqd.respond_with_string reqd response body ;
        Lwt.return_unit
      end @@ fun exn ->
        let res = Printexc.to_string exn in
        let headers = Headers.of_list
          [ "content-length", string_of_int (String.length res) ] in
        let response = Response.create ~headers `Internal_server_error in
        Reqd.respond_with_string reqd response res ;
        Lwt.return_unit in
    Lwt.async res
end

module Main
    (Database : Mirage_kv.RW)
    (Clock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6)
    (DNS : Dns_client_mirage.S with type Transport.stack = Stack.t)
= struct
  module Logs_reporter = Mirage_logs.Make(Clock)
  module Paf = Paf_mirage.Make(Time)(Stack.TCP)
  module LE = LE.Make(Time)(Stack)
  module Shortener = Webapp(Clock)(Database)
  module Nss = Ca_certs_nss.Make(Pclock)

  let ignore_error_handler _ ?request:_ _ _ = ()

  let get_certificates cfg stack dns =
    Paf.init ~port:80 (Stack.tcp stack) >>= fun t ->
    let service = Paf.http_service ~error_handler:ignore_error_handler
      (fun _flow -> LE.request_handler) in
    let stop = Lwt_switch.create () in
    let `Initialized th0 = Paf.serve ~stop service t in
    let th1 =
      let gethostbyname dns domain_name =
        DNS.gethostbyname dns domain_name >>= function
        | Ok ipv4 -> Lwt.return_ok (Ipaddr.V4 ipv4)
        | Error _ as err -> Lwt.return err in
      let authenticator = Result.get_ok (Nss.authenticator ()) in
      LE.provision_certificate
        ~production:true cfg
        (LE.ctx ~gethostbyname ~authenticator dns stack) >>= fun res ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return res in
    Lwt.both th0 th1 >>= function
    | ((), Error (`Msg err)) -> failwith err
    | ((), Ok certificates) -> Lwt.return certificates

  let start kv pclock _time stack dns =
    let open Lwt.Infix in
    let start_time = Ptime.v @@ Pclock.now_d_ps () in
    let host = Key_gen.host () in
    Logs_reporter.(create pclock |> run) @@ fun () ->
    (* solo5 requires us to use a block size of, at maximum, 512 *)
    (* Database.connect ~program_block_size:16 ~block_size:512 block >>= function
    | Error e -> Logs.err (fun f -> f "failed to initialize block-backed key-value store: %a" Database.pp_error e);
      Lwt.return_unit
    | Ok kv -> *)
    Logs.info (fun f -> f "block-backed key-value store up and running");
    match Key_gen.tls () with
    | false ->
      let request_handler _flow = Shortener.reply kv host start_time in
      let port = Option.value ~default:80 (Key_gen.port ()) in
      Paf.init ~port (Stack.tcp stack) >>= fun t ->
      let service = Paf.http_service ~error_handler:ignore_error_handler
        request_handler in
      let `Initialized th = Paf.serve service t in
      th
    | true ->
      let cfg =
        { LE.certificate_seed= None
        ; LE.certificate_key_type= `ED25519
        ; LE.certificate_key_bits= None
        ; LE.email= None
        ; LE.account_seed= None
        ; LE.account_key_type= `ED25519
        ; LE.account_key_bits= None
        ; LE.hostname= Key_gen.host ()
                       |> Domain_name.of_string_exn
                       |> Domain_name.host_exn } in
      let rec provision () =
        get_certificates cfg stack dns >>= fun certificates ->
        let tls = Tls.Config.server ~certificates () in
        let request_handler _flow = Shortener.reply kv host start_time in
        let port = Option.value ~default:443 (Key_gen.port ()) in
        Paf.init ~port (Stack.tcp stack) >>= fun service ->
        let https = Paf.https_service ~tls ~error_handler:ignore_error_handler
          request_handler in
        let stop = Lwt_switch.create () in
        let `Initialized th0 = Paf.serve ~stop https service in
        let expire () = Time.sleep_ns (Duration.of_day 80) >>= fun () ->
          Lwt_switch.turn_off stop in
        Lwt.pick [ th0; expire () ] >>= provision in
      provision ()
end
