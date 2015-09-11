(* ISO 9660 records *)

type strA (* A-Z, 0-9, !, '\"' % & '\'' ( ) * + , - . / : ; < = > ? *)

type strD (* A-Z, 0-9, _ *)

type primary_volume_descriptor = {
  system_id : string;
  volume_id : string;
  size : Int32.t;
  volume_set_size : int;
  volume_seqno : int;
  block_size : int;
  path_table_size : Int32.t;
  path_table_l_loc : Int32.t option;
  opt_path_table_l_loc : Int32.t option;
  path_table_m_loc : Int32.t option;
  opt_path_table_m_loc : Int32.t option
}

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

cstruct boot_record {
    uint8_t boot_system_id[32];
    uint8_t boot_id[32];
} as little_endian

cstruct pvd {
    uint8_t system_id[32];
    uint8_t volume_id[32];
    uint8_t unused1[8];
    uint32_t size_LSB;
    uint32_t size_MSB;
    uint8_t unused2[32];
    uint16_t vol_set_size_LSB;
    uint16_t vol_set_size_MSB;
    uint16_t vol_seq_no_LSB;
    uint16_t vol_seq_no_MSB;
    uint16_t block_size_LSB;
    uint16_t block_size_MSB;
    uint32_t path_table_size_LSB;
    uint32_t path_table_size_MSB;
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

let unmarshal_primary_volume_descriptor (buf : Cstruct.t) =
  let ty = get_volume_descriptor_ty buf in
  Printf.printf "ty=%d\n%!" ty;
  let pvd = get_volume_descriptor_data buf in
  let system_id = Cstruct.to_string (get_pvd_system_id pvd) in
  system_id



