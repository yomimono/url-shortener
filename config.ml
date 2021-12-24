(* this is a fancy interface between a filesystem and a web server *)

open Mirage

let main =
  foreign ~packages:[] ~keys:[] "Shortener.Main" (block @-> http @-> job)
