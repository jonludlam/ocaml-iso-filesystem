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

let rec print prefix entries =
  List.iter
    (function
      | (name, Isofs.File f) ->
        Printf.printf "%s%s (%s)\n" prefix name (get_description f)
      | (name, Isofs.Directory d) ->
        Printf.printf "%s%s\n" prefix name;
        print (Printf.sprintf "%s%s/" prefix name) d.Isofs.d_contents
    )
    entries

let th =
   Block.connect Sys.argv.(1)
   >>|= fun b ->
   Iso.connect b
   >>|= fun iso ->
   Iso.size iso (Sys.argv.(2))
   >>|= fun size ->
   Iso.read iso (Sys.argv.(2)) 0 (Int64.to_int size)
   >>|= fun result ->
   List.iter (fun x -> Printf.printf "%s" (Cstruct.to_string x)) result;
   Lwt.return (`Ok ())

let _ =
  Lwt_main.run th
