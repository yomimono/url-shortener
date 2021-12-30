open Lwt.Infix

module Webapp
    (KV : Mirage_kv.RW)
    (H : Cohttp_mirage.Server.S) = struct
  let reply (kv : KV.t) =
    let callback _connection request body =
      match Cohttp.Request.meth request with
      | `GET -> begin
        let path = Uri.path @@ Cohttp.Request.uri request in
        KV.get kv @@ Mirage_kv.Key.v path >>= function
        | Error (`Not_found k) ->
          Logs.debug (fun f -> f "key %a not found" Mirage_kv.Key.pp k);
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
      end
      | `POST -> begin
        let response = Cohttp.Response.make () in
        let body = Cohttp_lwt__.Body.empty in
        Lwt.return (response, body)
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
  module Shortener = Webapp(Database)(Http)

  let start block pclock _time http_server http_client =
    let open Lwt.Infix in
    Logs_reporter.(create pclock |> run) @@ fun () ->
    (* solo5 requires us to use a block size of, at maximum, 512 *)
    Database.connect ~program_block_size:16 ~block_size:512 block >>= function
    | Error e -> Logs.err (fun f -> f "failed to initialize block-backed key-value store: %a" Database.pp_error e);
      Lwt.return_unit
    | Ok kv ->
    Logs.info (fun f -> f "block-backed key-value store up and running");
    (*
    let rec provision () =
      LE.provision http_server http_client >>= fun certificates ->
      Logs.info (fun f -> f "got certificates from let's encrypt via acme");
      let tls_cfg = Tls.Config.server ~certificates () in
      let tls = `TLS (tls_cfg, `TCP 443) in
      let https =
        Logs.info (fun f -> f "(re-)initialized https listener");
        http_server tls @@ Shortener.reply
      in
      let expire = Time.sleep_ns @@ Duration.of_day 80 in
      Lwt.pick [
        https
      ; expire] >>= fun () ->
      provision ()
    in
    provision ()
       *)
    http_server (`TCP 80) @@ Shortener.reply kv
end
