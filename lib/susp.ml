

module Px = struct
  type t = {
    mode : Int32.t;
    links : Int32.t;
    uid : Int32.t;
    gid : Int32.t;
    serial : Int32.t;
  }

  cstruct px {
    uint8_t mode[8];
    uint8_t links[8];
    uint8_t uid[8];
    uint8_t gid[8];
    uint8_t serial[8];
  } as little_endian

  let unmarshal v =
    let mode = Multibyte.int32_of_lsb_msb (get_px_mode v) in
    let links = Multibyte.int32_of_lsb_msb (get_px_links v) in
    let uid = Multibyte.int32_of_lsb_msb (get_px_uid v) in
    let gid = Multibyte.int32_of_lsb_msb (get_px_gid v) in
    let serial = Multibyte.int32_of_lsb_msb (get_px_serial v) in
    { mode; links; uid; gid; serial }

end

module Pn = struct
  type t = {
    dev : Int64.t
  }

  cstruct pn {
    uint8_t high[8];
    uint8_t low[8];
  } as little_endian

  let unmarshal v =
    let high = Multibyte.int32_of_lsb_msb (get_pn_high v) in
    let low = Multibyte.int32_of_lsb_msb (get_pn_low v) in
    let open Int64 in
    let conv x = logand 0x00000000ffffffffL (of_int32 x) in
    let dev = add (shift_left (conv high) 32) (conv low) in
    { dev }

end

module Nm = struct
  type t = {
    is_current : bool;
    is_parent : bool;
    cont : bool;
    filename : string;
  }

  cstruct nm {
    uint8_t flags;
  } as little_endian

  let unmarshal v =
    let flags = get_nm_flags v in
    let is_current = (flags land 2 = 2) in
    let is_parent = (flags land 4 = 4) in
    let cont = (flags land 1 = 1) in
    let filename = Cstruct.to_string (Cstruct.sub v 1 (Cstruct.len v - 1)) in
    { is_current; is_parent; cont; filename }

end

module Ce = struct
  type t = {
    block_location : Int32.t;
    offset : Int32.t;
    length : Int32.t;
  }

  cstruct ce {
    uint8_t block_location[8];
    uint8_t offset[8];
    uint8_t length[8];
  } as little_endian

  let unmarshal v =
    let block_location = Multibyte.int32_of_lsb_msb (get_ce_block_location v) in
    let offset = Multibyte.int32_of_lsb_msb (get_ce_offset v) in
    let length = Multibyte.int32_of_lsb_msb (get_ce_length v) in
    { block_location; offset; length }

  let print ce =
    Printf.printf "{ block_location=%ld; offset=%ld; length=%ld }" ce.block_location ce.offset ce.length
end

type unhandled_entry = {
  signature : string;
  version : int;
  data : Cstruct.t;
}

cstruct susp {
    uint8_t signature[2];
    uint8_t len;
    uint8_t version;
} as little_endian

type t =
  | NM of Nm.t
  | PX of Px.t
  | CE of Ce.t
  | Unhandled of unhandled_entry

type error = [ `Invalid_SUSP_entry ]

let unmarshal v =
  let total_len = Cstruct.len v in
  let rec inner n acc =
    if n>=total_len-1 then `Ok acc else begin
      let susp_header = Cstruct.sub v n 4 in
      let signature = Cstruct.to_string (get_susp_signature susp_header) in
      let len = get_susp_len susp_header in
      if len<5 || n+len > total_len
      then `Error `Invalid_SUSP_entry
      else
        let version = get_susp_version susp_header in
        let data = Cstruct.sub v (n+4) (len-4) in
        let entry =
          match signature with
          | "NM" -> let nm = Nm.unmarshal data in Printf.printf "Got NM (name=%s)\n%!" nm.Nm.filename; NM (nm)
          | "PX" -> PX (Px.unmarshal data)
          | "CE" -> let ce = Ce.unmarshal data in Ce.print ce; CE ce
          | x -> Printf.printf "Unhandled: %s\n%!" x; Unhandled { signature; version; data }
        in
        inner (n+len) (entry :: acc)
    end
  in inner 0 []

let is_dot_or_dotdot susp =
  List.fold_left (fun acc entry -> match entry with | NM nm -> nm.Nm.is_current || nm.Nm.is_parent || acc | _ -> acc) false susp
