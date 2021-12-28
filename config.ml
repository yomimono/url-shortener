(* this is a fancy interface between a filesystem and a web server *)

open Mirage

let packages = [
  Functoria.package "letsencrypt";
  Functoria.package "uri";
  Functoria.package ~sublibs:["kv"] "chamelon";
  Functoria.package ~sublibs:["ocaml"] "digestif";
  Functoria.package ~sublibs:["ocaml"] "checkseum";
]

let stack = generic_stackv4v6 default_network
let http_srv = cohttp_server @@ conduit_direct stack

let block_imp = generic_block "shortener"

let main =
  foreign ~packages ~keys:[] "Shortener.Main" (block @-> pclock @-> http @-> job)

let () =
  register "shortener" [ main $ block_imp $ default_posix_clock $ http_srv ]
