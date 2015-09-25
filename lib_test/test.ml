(*
 * Copyright (C) 2015 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
   Block.connect "test.iso"
   >>|= fun b ->
   Iso.connect b
   >>|= fun iso ->
   print "/" iso.Iso.entries;
   Iso.KV_RO.size iso "/hello.txt"
   >>|= fun size ->
   Iso.KV_RO.read iso "/hello.txt" 0 (Int64.to_int size)
   >>|= fun result ->
   List.iter (fun x -> Printf.printf "%s" (Cstruct.to_string x)) result;
   Lwt.return (`Ok ())

let _ =
  Lwt_main.run th
