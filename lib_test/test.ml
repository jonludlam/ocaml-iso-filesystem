open OUnit
open Lwt
open Iso

module Iso = Isofs.Make(Block)(Io_page)

exception Error
let (>>|=) m f = m >>= function
  | `Error e -> fail Error
  | `Ok x -> f x
let th =
   Block.connect Sys.argv.(1)
   >>|= fun b ->
   Iso.connect b >>|=
   fun xs ->
   List.iter (function Isofs.File f -> Printf.printf "%s\n" f.Isofs.f_name | Isofs.Directory d -> Printf.printf "%s\n" d.Isofs.d_name) xs;
   Lwt.return (`Ok ())

let _ =
  Lwt_main.run th
