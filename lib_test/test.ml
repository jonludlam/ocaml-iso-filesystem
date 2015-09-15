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
   Iso.connect b

let _ =
  Lwt_main.run th
