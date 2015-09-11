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
  Lwt.return ()

let _ =
  Lwt_main.run th
