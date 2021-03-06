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

module type BLOCK_DEVICE = V1.BLOCK
  with type page_aligned_buffer = Cstruct.t
   and type 'a io = 'a Lwt.t

module type IO_PAGE = sig
  val get_buf : ?n:int -> unit -> Cstruct.t
  (** [get_buf ~n ()] allocates and returns a memory block of [n] pages,
      represented as a {!Cstruct.t}. If there is not enough memory,
      an [Out_of_memory] exception is raised. *)
end

module type FS = V1.FS
with type page_aligned_buffer = Cstruct.t
and type 'a io = 'a Lwt.t
