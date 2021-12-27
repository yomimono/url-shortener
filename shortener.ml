module Main
    (Block : Mirage_block.S)
    (Clock : Mirage_clock.PCLOCK)
    (Http : Cohttp_mirage.Server.S)
= struct
  module Logs_reporter = Mirage_logs.Make(Clock)
  module Kv = Kv.Make(Block)(Clock)

  let start block pclock _http =
    let open Lwt.Infix in
    Logs_reporter.(create pclock |> run) @@ fun () ->
    Kv.connect ~program_block_size:16 ~block_size:4096 block >>= fun kv ->
    Lwt.return_unit
end
