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

  let read_directory_entries sector =
    let len = Cstruct.len sector in
    let rec inner n acc =
      let entry_opt = Pathtable.maybe_unmarshal_directory
          (Cstruct.sub sector n (len - n)) in
      match entry_opt with
      | `Ok None ->
        `Ok acc
      | `Ok (Some entry) ->
        inner (n+entry.Pathtable.len) (entry::acc)
      | `Error _ as x -> x
    in
    inner 0 []

  let rec read_whole_directory device dir =
    let name = Pathtable.get_filename dir in
    let sector = alloc 2048 in
    let read_sector n =
      B.read device Int64.(mul n 4L) [ sector ]
      >>|= fun () ->
      Lwt.return (read_directory_entries sector)
    in
    let total_sectors = Int64.(div (of_int32 dir.Pathtable.data_len) 2048L) in
    let rec inner n acc =
      if n=total_sectors
      then Lwt.return (`Ok (List.concat (List.rev acc)))
      else begin
        read_sector (Int64.add n (Int64.of_int32 dir.Pathtable.location))
        >>|= fun entries ->
        inner Int64.(add n 1L) (entries::acc)
      end
    in
    inner 0L []
    >>|= fun entries ->
    let convert_entry acc entry =
      acc >>|= fun entries ->
      let open Pathtable in
      let name = Pathtable.get_filename entry in
      if name="." || name=".." || Susp.is_dot_or_dotdot entry.Pathtable.susp
      then Lwt.return (`Ok entries)
      else
      if List.mem Directory entry.flags
      then read_whole_directory device entry >>|= fun entry -> Lwt.return (`Ok (entry::entries))
      else Lwt.return (`Ok ((name, File { f_contents=OnDisk (dir.Pathtable.location, dir.Pathtable.data_len) })::entries))
    in
    List.fold_left convert_entry (Lwt.return (`Ok [])) entries
    >>|= fun entries ->
    Lwt.return (`Ok (name, Directory { d_contents=entries }))

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
    let pvd = List.fold_left (fun acc v ->
        match v with Descriptors.Primary_volume_descriptor pvd -> Some pvd | _ -> acc) None descriptors in
    (match pvd with
     | Some pvd ->
       begin
         read_whole_directory device pvd.Descriptors.Primary.root_dir
         >>|= fun x ->
         match x with
         | _, Directory {d_contents=entries} -> Lwt.return (`Ok {device; entries})
         | _, _ -> Lwt.return (`Error (`Unknown "Root directory wasn't a directory...?"))
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
      B.read t.device sector [pages]
      >>|= fun () ->
      Lwt.return (`Ok [Cstruct.sub pages offset length])
    | _ ->
      Lwt.return (`Error (`Unknown_error "No such file"))

  let listdir t key =
    begin
      try
        let res = `Ok (locate t.entries key) in
        Lwt.return res
      with
      | FileNotFound x ->
        Lwt.return (`Error (`Unknown_error "File not found"))
    end
    >>|= function
    | File _ ->
      Lwt.return (`Error (`Not_a_directory key))
    | Directory d ->
      let contents = List.map fst d.d_contents in
      Lwt.return (`Ok contents)

end
