open OUnit
open Lwt
open Iso

module Iso = Isofs.Make(Block)(Io_page)

let get_description f =
  let open Isofs in
  match f.f_contents with
  | OnDisk (location, length) -> Printf.sprintf "%ld, %ld" (Int32.mul 2048l location) length
  | Immediate s -> Printf.sprintf "'%s'" s

exception Error
let (>>|=) m f = m >>= function
  | `Error e -> fail Error
  | `Ok x -> f x
let th =
   Block.connect Sys.argv.(1)
   >>|= fun b ->
   Iso.connect b >>|=
   fun xs ->
   List.iter (function Isofs.File f -> Printf.printf "%s (%s)\n" f.Isofs.f_name (get_description f)  | Isofs.Directory d -> Printf.printf "%s\n" d.Isofs.d_name) xs;
   Lwt.return (`Ok ())

let _ =
  Lwt_main.run th
