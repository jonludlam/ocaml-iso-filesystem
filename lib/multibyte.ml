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

(* Some Iso9660 are stored as both big and little endian values.
   This exception is raised if they're not the same (when non-zero) *)

exception Invalid_coding

let int32_of_lsb_msb v =
  let lsb = Cstruct.LE.get_uint32 v 0 in
  let msb = Cstruct.BE.get_uint32 v 4 in
  match lsb, msb with
  | x, 0l -> x
  | 0l, x -> x
  | x, y -> if x<>y then raise Invalid_coding else x

let int16_of_lsb_msb v =
  let lsb = Cstruct.LE.get_uint16 v 0 in
  let msb = Cstruct.BE.get_uint16 v 2 in
  match lsb, msb with
  | x, 0 -> x
  | 0, x -> x
  | x, y -> if x<>y then raise Invalid_coding else x

let roundup n = if n mod 2 = 1 then n+1 else n

