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



module Make (B: S.BLOCK_DEVICE
                with type 'a io = 'a Lwt.t
                and type page_aligned_buffer = Cstruct.t)(M: S.IO_PAGE) = struct

  exception Block_device_error of B.error
  let (>>|=) m f = m >>= function
    | `Error e -> fail (Block_device_error e)
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
    B.read device (Int64.div 32768L 512L) [ sector ]
    >>|= fun () ->
    match Records.unmarshal_primary_volume_descriptor sector with
    | None -> failwith "no pvd"
    | Some pvd ->
      Printf.printf "here...\n%!";
      let rec get_dirs prefix lba n =
        B.read device Int64.(mul 4L (of_int32 lba)) [ sector ]
        >>|= fun () ->
        let dir_opt = Records.maybe_unmarshal_directory (Cstruct.sub sector n (2048 - n)) in
        begin
          match dir_opt with
          | None -> Lwt.return ()
          | Some dir -> begin
              let susp_records = String.concat "," (List.map (fun susp -> susp.Records.signature) dir.Records.susp) in
              Printf.printf "%s%s %ld %d (%s) %ld %ld\n" prefix dir.Records.filename lba n susp_records dir.Records.location dir.Records.data_len;
              (if List.mem Records.Directory dir.Records.flags && (String.length dir.Records.filename > 1)
              then get_dirs (Printf.sprintf "%s  " prefix) dir.Records.location 0
              else Lwt.return ()) >>= fun () ->
              get_dirs prefix lba (n+dir.Records.len)
            end
        end
      in get_dirs "" Records.(pvd.root_dir.location) 0

end
