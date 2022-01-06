open Lwt.Infix

module Webapp
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RW)
    (H : Cohttp_mirage.Server.S) = struct

  let reserved = [ "/uptime"; "/new"; "/status"; "/"; "/favicon.ico" ]

  let not_found = (
    Cohttp.Response.make ~status:Cohttp.Code.(`Not_found) (),
    Cohttp_lwt__.Body.of_string "Not found")

  let ise = (
    Cohttp.Response.make ~status:Cohttp.Code.(`Internal_server_error) (),
    Cohttp_lwt__.Body.of_string "Internal server error")

  let bad_request = (
    Cohttp.Response.make ~status:Cohttp.Code.(`Bad_request) (),
    Cohttp_lwt__.Body.of_string "Bad request")

  let slash =
    let form = "<form method=\"POST\" action=\"/new\">
      <label>Short name: 
        <input type=\"text\" required=\"true\" id=\"short\" name=\"short_name\" />
      </label>
      <label> URL:
        <input type=\"text\" required=\"true\" id=\"target\" name=\"url\"/>
      </label>
      <button type=\"submit\">Submit</button>
      </form>"
    in
    (Cohttp.Response.make ~status:Cohttp.Code.(`OK) (),
    Cohttp_lwt__.Body.of_string form)

  let uptime start_time =
    let response = Cohttp.Response.make ~status:Cohttp.Code.(`OK) () in
    let span = Ptime.Span.sub (Ptime.Span.v @@ Pclock.now_d_ps ()) (Ptime.to_span start_time) in
    let s = Format.asprintf "%a (since %a)" Ptime.Span.pp span (Ptime.pp_human ()) start_time in
    (response, Cohttp_lwt__.Body.of_string s)

  let get_reserved start_time path =
    if String.equal path "/uptime" then uptime start_time
    else if String.equal path "/favicon.ico" then not_found
    else if String.equal path "/" then slash
    else not_found

  let get_from_database kv path =
    KV.get kv @@ Mirage_kv.Key.v path >>= function
    | Error (`Not_found k) ->
      let response = Cohttp.Response.make ~status:Cohttp.Code.(`Not_found) () in
      let body = Cohttp_lwt__.Body.of_string "Not found" in
      Lwt.return (response, body)
    | Error e ->
      Logs.err (fun f -> f "error %a fetching a key from the database" KV.pp_error e);
      let response = Cohttp.Response.make ~status:Cohttp.Code.(`Internal_server_error) () in
      let body = Cohttp_lwt__.Body.of_string "Internal Server Error" in
      Lwt.return (response, body)
    | Ok data ->
      let loc = Cohttp.Header.init_with "Location" data in
      let response = Cohttp.Response.make ~status:Cohttp.Code.(`Temporary_redirect) ~headers:loc () in
      let body = Cohttp_lwt__.Body.empty in
      Lwt.return (response, body)

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
    (* arbitrarily, shortcuts cannot be longer than 128 characters *)
    if String.length path > 128 then Lwt.return bad_request
    else begin
      KV.exists kv @@ Mirage_kv.Key.v path >>= function
      | Error e ->
        Logs.err (fun f -> f "error %a trying to post a url" KV.pp_error e);
        Lwt.return ise
      | Ok (Some _) ->
        let response = Cohttp.Response.make ~status:Cohttp.Code.(`Conflict) () in
        let body = Cohttp_lwt__.Body.of_string "there's already a URL set there. Try choosing another" in
        Lwt.return (response, body)
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
            let response = Cohttp.Response.make ~status:Cohttp.Code.(`Created) () in
            let response_body = Cohttp_lwt__.Body.of_string "Success! Your shortcut has been created." in
            Lwt.return (response, response_body)
    end

  let reply (kv : KV.t) hostname start_time =
    let callback _connection request body =
      match Cohttp.Request.meth request with
      | `GET -> begin
        let path = Uri.path @@ Uri.canonicalize @@ Cohttp.Request.uri request in
        if List.mem path reserved then Lwt.return @@ get_reserved start_time path
        else get_from_database kv path
      end
      | `POST -> begin
          Cohttp_lwt__.Body.to_form body >>= fun form_entries ->
          match List.assoc_opt "short_name" form_entries, List.assoc_opt "url" form_entries with
          | Some (path::[]), Some (url::[]) when not @@ List.mem path reserved -> maybe_set kv hostname path url
          | _, _ -> Lwt.return bad_request
        end
      | _ ->
        let response = Cohttp.Response.make ~status:Cohttp.Code.(`Method_not_allowed) () in
        let body = Cohttp_lwt__.Body.empty in
        Lwt.return (response, body)
    in
    H.make ~conn_closed:(fun _ -> ()) ~callback ()
end

module Main
    (Block : Mirage_block.S)
    (Clock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Http : Cohttp_mirage.Server.S)
    (Client : Cohttp_lwt.S.Client)
= struct
  module Logs_reporter = Mirage_logs.Make(Clock)
  module LE = Le.Make(Time)(Http)(Client)
  module Database = Kv.Make(Block)(Clock)
  module Shortener = Webapp(Clock)(Database)(Http)

  let start block pclock _time http_server http_client =
    let open Lwt.Infix in
    let start_time = Ptime.v @@ Pclock.now_d_ps () in
    let host = Key_gen.host () in
    Logs_reporter.(create pclock |> run) @@ fun () ->
    (* solo5 requires us to use a block size of, at maximum, 512 *)
    Database.connect ~program_block_size:16 ~block_size:512 block >>= function
    | Error e -> Logs.err (fun f -> f "failed to initialize block-backed key-value store: %a" Database.pp_error e);
      Lwt.return_unit
    | Ok kv ->
    Logs.info (fun f -> f "block-backed key-value store up and running");
    let rec provision () =
      LE.provision host http_server http_client >>= fun certificates ->
      Logs.info (fun f -> f "got certificates from let's encrypt via acme");
      let tls_cfg = Tls.Config.server ~certificates () in
      let tls = `TLS (tls_cfg, `TCP 443) in
      let tcp = `TCP 80 in
      let https =
        Logs.info (fun f -> f "(re-)initialized https listener");
        http_server tls @@ Shortener.reply kv host start_time
      in
      let http =
        Logs.info (fun f -> f "overwriting Let's Encrypt http listener with ours");
        http_server tcp @@ Shortener.reply kv host start_time
      in
      let expire = Time.sleep_ns @@ Duration.of_day 80 in
      Lwt.pick [
        https
      ; http
      ; expire] >>= fun () ->
      provision ()
    in
    provision ()
end
