(* ISO 9660 records *)

type strA (* A-Z, 0-9, !, '\"' % & '\'' ( ) * + , - . / : ; < = > ? *)

type strD (* A-Z, 0-9, _ *)

type primary_volume_descriptor = {
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
  volume_set_id : string;
  publisher_id : string;
  data_preparer_id : string;
  app_id : string;
  copyright : string;
  abstract_file_id : string;
  biblio_file_id : string;
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
    uint8_t unused0;
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
  let ty = int_to_volume_descriptor_type (get_volume_descriptor_ty buf) in
  match ty with
  | Some PRIMARY_VOLUME_DESCRIPTOR ->
    let pvd = get_volume_descriptor_data buf in
    let system_id = Cstruct.to_string (get_pvd_system_id pvd) in
    let volume_id = Cstruct.to_string (get_pvd_volume_id pvd) in
    let size = get_pvd_size_LSB pvd in
    let vol_set_size = get_pvd_vol_set_size_LSB pvd in
    let vol_seq_no = get_pvd_vol_seq_no_LSB pvd in
    let block_size = get_pvd_block_size_LSB pvd in
    let path_table_size = get_pvd_path_table_size_LSB pvd in
    let path_table_l_loc = get_pvd_l_path_table pvd in
    let opt_path_table_l_loc = get_pvd_opt_l_path_table pvd in
    let root_directory = get_pvd_root_directory pvd in
    let volume_set_id = Cstruct.to_string (get_pvd_volume_set_id pvd) in
    let publisher_id = Cstruct.to_string (get_pvd_publisher_id pvd) in
    let data_preparer_id = Cstruct.to_string (get_pvd_data_preparer_id pvd) in
    let app_id = Cstruct.to_string (get_pvd_app_id pvd) in
    let copyright = Cstruct.to_string (get_pvd_copyright pvd) in
    let abstract_file_id = Cstruct.to_string (get_pvd_abstract_file_id pvd) in
    let biblio_file_id = Cstruct.to_string (get_pvd_biblio_file_id pvd) in
    Some { system_id; volume_id; size; vol_set_size; vol_seq_no; block_size;
      path_table_size; path_table_l_loc = Some path_table_l_loc; opt_path_table_l_loc = Some opt_path_table_l_loc;
      path_table_m_loc = None; opt_path_table_m_loc = None;
      volume_set_id; publisher_id; data_preparer_id; app_id; copyright; abstract_file_id;
      biblio_file_id }
  | Some BOOT_RECORD ->
    Printf.printf "boot record\n%!";
    None
  | Some SUPPLEMENTARY_VOLUME_DESCRIPTOR ->
    Printf.printf "supp\n%!";
    None
  | Some VOLUME_PARTITION_DESCRIPTOR ->
    Printf.printf "vol_part\n%!";
    None
  | Some VOLUME_DESCRIPTOR_SET_TERMINATOR ->
    None
  | None ->
    failwith "Unknown type!"

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
  List.iter (fun (k,v) -> Printf.printf "%s: %s\n" k v) fields
