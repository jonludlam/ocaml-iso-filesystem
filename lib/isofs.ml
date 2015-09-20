open Lwt

type file_contents =
| Immediate of string
| OnDisk of Int32.t * Int32.t (* Location, length *)

type file = {
  f_contents : file_contents;
}

and dir = {
  d_contents : (string * entry) list;
}

and entry =
  | Directory of dir
  | File of file

exception FileNotFound of string

let locate entries filename =
  if filename="" then raise (FileNotFound filename);

  let fullpath = Stringext.split filename ~on:'/' in

  (* Strip off leading '/' from path *)
  let relpath = match List.hd fullpath with "" -> List.tl fullpath | _ -> fullpath in

  let rec search entries path =
    let cur = List.hd path in
    let entry =
      try
        List.assoc cur entries
      with Not_found ->
        raise (FileNotFound filename)
    in
    match List.tl path, entry with
    | [], _ -> entry
    | rest, Directory d -> search d.d_contents rest
    | _, _ -> raise (FileNotFound filename)
  in
  search entries relpath


exception No_pvd_found

module Make (B: S.BLOCK_DEVICE
                with type 'a io = 'a Lwt.t
                and type page_aligned_buffer = Cstruct.t)(M: S.IO_PAGE) = struct

  type t = {
    device : B.t;
    entries : (string * entry) list;
  }

  type error =
    [ B.error
    | `Unknown_volume_descriptor_type
    | `Invalid_primary_volume_descriptor
    | `Invalid_volume_descriptor_id
    | `Invalid_SUSP_entry
    | `Unknown_error of string]

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
    | `Error `Unknown_error _ as z -> z

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
                  let name = Pathtable.get_filename dir in
                  if List.mem Pathtable.Directory dir.Pathtable.flags && (not (Susp.is_dot_or_dotdot dir.Pathtable.susp)) && not (List.mem dir.Pathtable.location seen)
                  then
                    get_dirs dir.Pathtable.location 0 (lba::seen) >>|=
                    fun list -> Lwt.return (`Ok [name, Directory { d_contents=list; }])
                  else Lwt.return (`Ok [name, File { f_contents=OnDisk (dir.Pathtable.location, dir.Pathtable.data_len) } ] ) end
                >>|= fun entry ->
                get_dirs lba (n+dir.Pathtable.len) seen
                >>|= fun other_entries ->
                Lwt.return (`Ok (entry @ other_entries))
            end
        in
        get_dirs Records.(pvd.root_dir.location) 0 [pvd.root_dir.location] >>|=
        fun entries ->
        Lwt.return (`Ok { device; entries})
      end
    | None ->
      Lwt.return (`Error (`Unknown "bah"))
    )

  let size t key =
    begin
      try
        let res = `Ok (locate t.entries key) in
        Lwt.return res
      with
      | FileNotFound x ->
        Printf.printf "file not found: %s\n%!" x;
        Lwt.return (`Error (`Unknown_error "File not found"))
    end
    >>|= function
    | File { f_contents = OnDisk (loc, len) } ->
      Lwt.return (`Ok (Int64.of_int32 len))
    | _ ->
      Lwt.return (`Error (`Unknown_error "No such file"))

  let read t key offset length =
    begin
      try
        let res = `Ok (locate t.entries key) in
        Lwt.return res
      with
      | FileNotFound x ->
        Lwt.return (`Error (`Unknown_error "File not found"))
    end
    >>|= function
    | File { f_contents = OnDisk (loc, len) } ->
      let rounded_up_size = 2048 * ((Int32.to_int len + 2047) / 2048) in
      let pages = alloc rounded_up_size in
      let sector = Int64.mul 4L (Int64.of_int32 loc) in
      Printf.printf "About to read from sector: %Ld (allocated %d bytes)\n%!" sector rounded_up_size;
      B.read t.device sector [pages]
      >>|= fun () ->
      Printf.printf "OK! %s\n%!" (Cstruct.to_string pages);
      Lwt.return (`Ok [Cstruct.sub pages offset length])
    | _ ->
      Lwt.return (`Error (`Unknown_error "No such file"))

end
