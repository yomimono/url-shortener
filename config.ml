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
let conduit = conduit_direct stack
let http_srv = cohttp_server conduit
let http_client_imp = cohttp_client (resolver_dns stack) conduit
let block_imp = block_of_file "shortener"

let main =
  foreign ~packages ~keys:[] "Shortener.Main" (block @-> pclock @-> time @-> http @-> http_client @-> job)

let () =
  register "shortener" [ main $ block_imp $ default_posix_clock $ default_time $ http_srv $ http_client_imp ]
