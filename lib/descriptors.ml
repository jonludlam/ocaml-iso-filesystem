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

open Result

let identifier_val = "CD001"

cenum volume_descriptor_type {
    BOOT_RECORD=0;
    PRIMARY_VOLUME_DESCRIPTOR=1;
    SUPPLEMENTARY_VOLUME_DESCRIPTOR=2;
    VOLUME_PARTITION_DESCRIPTOR=3;
    VOLUME_DESCRIPTOR_SET_TERMINATOR=255;
} as uint8_t

cstruct volume_descriptor {
    uint8_t ty;
    uint8_t id[5];
    uint8_t version;
    uint8_t data[2041];
} as little_endian


module Primary = struct
  type t = {
    system_id : string;
    volume_id : string;
    size : Int32.t;
    vol_set_size : int;
    vol_seq_no : int;
    block_size : int;
    path_table_size : Int32.t;
    path_table_l_loc : Int32.t option;
    opt_path_table_l_loc : Int32.t option;
    path_table_m_loc : Int32.t option;
    opt_path_table_m_loc : Int32.t option;
    root_dir : Pathtable.dir;
    volume_set_id : string;
    publisher_id : string;
    data_preparer_id : string;
    app_id : string;
    copyright : string;
    abstract_file_id : string;
    biblio_file_id : string;
  }

  cstruct pvd {
    uint8_t unused0;
    uint8_t system_id[32];
    uint8_t volume_id[32];
    uint8_t unused1[8];
    uint8_t size_lsb_msb[8];
    uint8_t unused2[32];
    uint8_t vol_set_size_lsb_msb[4];
    uint8_t vol_seq_no_lsb_msb[4];
    uint8_t block_size_lsb_msb[4];
    uint8_t path_table_size_lsb_msb[8];
    uint32_t l_path_table;
    uint32_t opt_l_path_table;
    uint32_t m_path_table;
    uint32_t opt_m_path_table;
    uint8_t root_directory[34];
    uint8_t volume_set_id[128];
    uint8_t publisher_id[128];
    uint8_t data_preparer_id[128];
    uint8_t app_id[128];
    uint8_t copyright[38];
    uint8_t abstract_file_id[36];
    uint8_t biblio_file_id[37];
    uint8_t ctime[17];
    uint8_t mtime[17];
    uint8_t xtime[17];
    uint8_t etime[17];
    uint8_t file_structure_version;
    uint8_t unused3;
  } as little_endian

  let unmarshal pvd =
    let open Multibyte in
    let system_id = Cstruct.to_string (get_pvd_system_id pvd) in
    let volume_id = Cstruct.to_string (get_pvd_volume_id pvd) in
    let size = int32_of_lsb_msb (get_pvd_size_lsb_msb pvd) in
    let vol_set_size = int16_of_lsb_msb (get_pvd_vol_set_size_lsb_msb pvd) in
    let vol_seq_no = int16_of_lsb_msb (get_pvd_vol_seq_no_lsb_msb pvd) in
    let block_size = int16_of_lsb_msb (get_pvd_block_size_lsb_msb pvd) in
    let path_table_size = int32_of_lsb_msb (get_pvd_path_table_size_lsb_msb pvd) in
    let path_table_l_loc = get_pvd_l_path_table pvd in
    let opt_path_table_l_loc = get_pvd_opt_l_path_table pvd in
    Pathtable.unmarshal_directory (get_pvd_root_directory pvd)
    >>= fun root_dir ->
    let volume_set_id = Cstruct.to_string (get_pvd_volume_set_id pvd) in
    let publisher_id = Cstruct.to_string (get_pvd_publisher_id pvd) in
    let data_preparer_id = Cstruct.to_string (get_pvd_data_preparer_id pvd) in
    let app_id = Cstruct.to_string (get_pvd_app_id pvd) in
    let copyright = Cstruct.to_string (get_pvd_copyright pvd) in
    let abstract_file_id = Cstruct.to_string (get_pvd_abstract_file_id pvd) in
    let biblio_file_id = Cstruct.to_string (get_pvd_biblio_file_id pvd) in
    `Ok { system_id; volume_id; size; vol_set_size; vol_seq_no; block_size;
      path_table_size; path_table_l_loc = Some path_table_l_loc;
      opt_path_table_l_loc = Some opt_path_table_l_loc; path_table_m_loc = None;
      opt_path_table_m_loc = None; root_dir; volume_set_id; publisher_id;
      data_preparer_id; app_id; copyright; abstract_file_id; biblio_file_id }

  let print_pvd pvd =
    let string_of_int32_opt io =
      match io with
      | None -> "None"
      | Some i -> Printf.sprintf "%ld" i
    in
    let fields = ["system_id", pvd.system_id;
                  "volume_id", pvd.volume_id;
                  "size", Printf.sprintf "%ld" pvd.size;
                  "vol_set_size", string_of_int pvd.vol_set_size;
                  "vol_seq_no", string_of_int pvd.vol_seq_no;
                  "block_size", string_of_int pvd.block_size;
                  "path_table_size", Printf.sprintf "%ld" pvd.path_table_size;
                  "path_table_l_loc", string_of_int32_opt pvd.path_table_l_loc;
                  "opt_path_table_l_loc", string_of_int32_opt pvd.opt_path_table_l_loc;
                  "path_table_m_loc", string_of_int32_opt pvd.path_table_m_loc;
                  "opt_path_table_m_loc", string_of_int32_opt pvd.opt_path_table_m_loc;
                  "volume_set_id", pvd.volume_set_id;
                  "publisher_id", pvd.publisher_id;
                  "data_preparer_id", pvd.data_preparer_id;
                  "app_id", pvd.app_id;
                  "copyright", pvd.copyright;
                  "abstract_file_id", pvd.abstract_file_id;
                  "biblio_file_id", pvd.biblio_file_id;
                 ]
    in
    List.iter (fun (k,v) -> Printf.printf "%s: %s\n" k v) fields;
    Printf.printf "root_dir:\n";
    Pathtable.print_directory pvd.root_dir

end

module Boot = struct
  type t = {
    system_id : string;
    boot_id : string;
  }

  cstruct boot_record {
    uint8_t boot_system_id[32];
    uint8_t boot_id[32];
  } as little_endian
end


type volume_descriptor =
  | Primary_volume_descriptor of Primary.t
  | Boot_record
  | Supplementary_volume_descriptor
  | Volume_partition_descriptor
  | Volume_descriptor_set_terminator

let unmarshal (buf : Cstruct.t) =
  let ty = int_to_volume_descriptor_type (get_volume_descriptor_ty buf) in
  let id = Cstruct.to_string (get_volume_descriptor_id buf) in
  if id <> identifier_val then (`Error `Invalid_volume_descriptor_id) else
  match ty with
  | Some PRIMARY_VOLUME_DESCRIPTOR ->
    begin
      try
        let pvd = get_volume_descriptor_data buf in
        Primary.unmarshal pvd
        >>= fun result ->
        `Ok (Primary_volume_descriptor result)
      with e ->
        `Error `Invalid_primary_volume_descriptor
    end
  | Some BOOT_RECORD ->
    `Ok Boot_record
  | Some SUPPLEMENTARY_VOLUME_DESCRIPTOR ->
    `Ok Supplementary_volume_descriptor
  | Some VOLUME_PARTITION_DESCRIPTOR ->
    `Ok Volume_partition_descriptor
  | Some VOLUME_DESCRIPTOR_SET_TERMINATOR ->
    `Ok Volume_descriptor_set_terminator
  | None ->
    `Error `Unknown_volume_descriptor_type



