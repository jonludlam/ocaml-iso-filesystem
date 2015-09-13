open OUnit
open Lwt
open Iso
open Block

let alloc bytes =
  let pages = Io_page.(to_cstruct (get ((bytes + 4095) / 4096))) in
  Cstruct.sub pages 0 bytes

let read_sector filename =
  Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
  let buf = alloc 512 in
  really_read fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  return buf

let read_whole_file filename =
  Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
  let size = (Unix.stat filename).Unix.st_size in
  let buf = alloc size in
  really_read fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  return buf

let th =
  read_whole_file Sys.argv.(1)
  >>= fun x ->
  let sect = Cstruct.sub x 32768 2048 in
  Records.unmarshal_primary_volume_descriptor sect |>
  (function | Some pvd -> Records.print_pvd pvd | None -> ());
  let sect = Cstruct.sub x (32768+2048) 2048 in
  Records.unmarshal_primary_volume_descriptor sect |>
  (function | None -> Printf.printf "Got none!\n%!" | _ -> failwith "ack");
  let rec get_dirs prefix lba n =
    let sect = Cstruct.sub x (lba*2048) 2048 in
    let dir_opt = Records.maybe_unmarshal_directory (Cstruct.sub sect n (2048 - n)) in
    match dir_opt with
    | None -> ()
    | Some dir -> begin
        let susp_records = String.concat "," (List.map (fun susp -> susp.Records.signature) dir.Records.susp) in
        Printf.printf "%s%s %d %d (%s)\n" prefix dir.Records.filename lba n susp_records;
        if dir.Records.flags=2 && (String.length dir.Records.filename > 1)
        then get_dirs (Printf.sprintf "%s  " prefix) (Int32.to_int dir.Records.location) 0;
        get_dirs prefix lba (n+dir.Records.len)
      end
  in get_dirs "" 29 0;
  Lwt.return ()

let _ =
  Lwt_main.run th
