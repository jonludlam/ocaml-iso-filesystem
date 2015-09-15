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
