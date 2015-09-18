open Lwt

type file_contents =
| Immediate of string
| OnDisk of Int32.t * Int32.t (* Location, length *)

type file = {
  f_name : string;
  f_contents : file_contents;
}

and dir = {
  d_name : string;
  d_contents : entry list;
}

and entry =
  | Directory of dir
  | File of file

exception No_pvd_found

module Make (B: S.BLOCK_DEVICE
                with type 'a io = 'a Lwt.t
                and type page_aligned_buffer = Cstruct.t)(M: S.IO_PAGE) = struct

  type error =
    [ B.error
    | `Unknown_volume_descriptor_type
    | `Invalid_primary_volume_descriptor
    | `Invalid_volume_descriptor_id
    | `Invalid_SUSP_entry ]

  type ('a, 'b) result = [ `Ok of 'a | `Error of 'b ]

  let openerr : ('a, [< error ]) result -> ('a, [>error]) result =
    function
    | `Ok x as z -> z
    | `Error `Disconnected as z -> z
    | `Error `Is_read_only as z -> z
    | `Error `Unimplemented as z -> z
    | `Error `Unknown _ as z -> z
    | `Error `Unknown_volume_descriptor_type as z -> z
    | `Error `Invalid_primary_volume_descriptor as z -> z
    | `Error `Invalid_volume_descriptor_id as z -> z
    | `Error `Invalid_SUSP_entry as z -> z

  exception Block_device_error of B.error
  let ((>>|=) : ('a, [< error]) result Lwt.t -> ('a -> ('c, [> error]) result Lwt.t) -> ('c, [> error]) result Lwt.t) = fun m f -> m >>= function
    | `Error x as z -> Lwt.return (openerr z)
    | `Ok x -> f x

  let alloc bytes =
    let pages = M.get_buf ~n:((bytes + 4095) / 4096) () in
    Cstruct.sub pages 0 bytes

  let read device sector =
    let buf = alloc Records.sector_size in
    let sec = Int64.of_int32 Int32.(mul sector 4l) in
    B.read device sec [buf]

  let connect device =
    let page = alloc 4096 in
    B.get_info device >>= fun info ->
    let sector = Cstruct.sub page 0 Records.sector_size in
    let rec handle_volume_descriptors sector_num acc =
      B.read device (Int64.mul sector_num 4L) [ sector ] >>= fun x -> Lwt.return (openerr x)
      >>|= fun () ->
      match Descriptors.unmarshal sector with
      | `Ok Descriptors.Volume_descriptor_set_terminator -> Lwt.return (`Ok acc)
      | `Ok other -> handle_volume_descriptors (Int64.add 1L sector_num) (other::acc)
      | _ -> Lwt.return (`Ok acc)
    in
    handle_volume_descriptors 16L []
    >>|= fun descriptors ->
    Printf.printf "got descriptors\n%!";
    let pvd = List.fold_left (fun acc v ->
      match v with Descriptors.Primary_volume_descriptor pvd -> Some pvd | _ -> acc) None descriptors in
    (match pvd with
    | Some pvd ->
      begin
        Printf.printf "got pvd\n%!";
        let rec get_dirs lba n seen =
          B.read device Int64.(mul 4L (of_int32 lba)) [ sector ]
          >>|= fun () ->
          Lwt.return (Pathtable.maybe_unmarshal_directory (Cstruct.sub sector n (2048 - n)))
          >>|= fun dir_opt ->
            begin
              match dir_opt with
              | None -> Lwt.return (`Ok [])
              | Some dir -> begin
                  if List.mem Pathtable.Directory dir.Pathtable.flags && (not (Susp.is_dot_or_dotdot dir.Pathtable.susp)) && not (List.mem dir.Pathtable.location seen)
                  then
                    get_dirs dir.Pathtable.location 0 (lba::seen) >>|=
                    fun list -> Lwt.return (`Ok [Directory { d_contents=list; d_name=dir.Pathtable.filename }])
                  else Lwt.return (`Ok [File { f_name=dir.Pathtable.filename; f_contents=OnDisk (dir.Pathtable.location, dir.Pathtable.data_len) } ] ) end
                >>|= fun entry ->
                get_dirs lba (n+dir.Pathtable.len) seen
                >>|= fun other_entries ->
                Lwt.return (`Ok (entry @ other_entries))
            end
        in
        get_dirs Records.(pvd.root_dir.location) 0 [pvd.root_dir.location]
      end
    | None ->
      Lwt.return (`Error (`Unknown "bah"))
    )



end
