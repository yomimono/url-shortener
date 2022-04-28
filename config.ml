(* this is a fancy interface between a filesystem and a web server *)

open Mirage

let packages = [
  package "uri";
  package ~sublibs:["ocaml"] "digestif";
  package ~sublibs:["ocaml"] "checkseum";
  package "paf";
  package "paf-le";
  package "paf" ~sublibs:[ "mirage" ];
  package "multipart_form-lwt";
]

let host =
  let doc = Key.Arg.info ~doc:"Fully-qualified domain name for the server. Certificates will be requested from Let's Encrypt for this name." ["host"] in
  Key.(create "host" Arg.(required string doc))

let tls =
  let doc = Key.Arg.info ~doc:"Bootstrap with a Let's encrypt certificate and an HTTPS server." ["tls"] in
  Key.(create "tls" Arg.(opt bool false doc))

let port =
  let doc = Key.Arg.info ~doc:"Port where the HTTP(S) must listen." ["port"] in
  Key.(create "port" Arg.(opt (some int) None doc))

let program_block_size =
  let doc = Key.Arg.info ~doc:"Program block size." [ "program-block-size" ] in
  Key.(create "program_block_size" Arg.(opt int 16 doc))

let keys = [ Key.v host; Key.v tls; Key.v port ]

let main =
  foreign ~packages ~keys "Shortener.Main" (kv_rw @-> pclock @-> time @-> stackv4v6 @-> dns_client @-> job)

let stack = generic_stackv4v6 default_network
let block = chamelon ~program_block_size (block_of_file "db")
let dns = generic_dns_client stack

let () =
  register "shortener" [ main $ block $ default_posix_clock $ default_time $ stack $ dns ]
