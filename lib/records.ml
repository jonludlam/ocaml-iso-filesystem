(* ISO 9660 records *)

type strA (* A-Z, 0-9, !, '\"' % & '\'' ( ) * + , - . / : ; < = > ? *)

type strD (* A-Z, 0-9, _ *)

type flag = | Hidden | Directory | AssociatedFile | Format | Perms | NotFinal

let sector_size = 2048

let string_of_flag = function
  | Hidden -> "Hidden"
  | Directory -> "Directory"
  | AssociatedFile -> "AssociatedFile"
  | Format -> "Format"
  | Perms -> "Perms"
  | NotFinal -> "NotFinal"

type path_table_entry = {
  pte_location : Int32.t;
  pte_parent : int;
  pte_name : string;
}

type susp_entry = {
  signature : string;
  version : int;
  data : Cstruct.t;
}

type dir = {
  len : int;
  ext_len : int;
  location : Int32.t;
  data_len : Int32.t;
  date : unit;
  flags : flag list;
  file_unit_size : int;
  gap_size : int;
  vol_seq : int;
  filename : string;
  susp : susp_entry list;
}

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
  root_dir : dir;
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

(* Some Iso9660 are stored as both big and little endian values.
   This exception is raised if they're not the same (when non-zero) *)
exception Invalid_coding

let int32_of_lsb_msb v =
  let lsb = Cstruct.LE.get_uint32 v 0 in
  let msb = Cstruct.BE.get_uint32 v 4 in
  match lsb, msb with
  | x, 0l -> x
  | 0l, x -> x
  | x, y -> if x<>y then raise Invalid_coding else x

let int16_of_lsb_msb v =
  let lsb = Cstruct.LE.get_uint16 v 0 in
  let msb = Cstruct.BE.get_uint16 v 2 in
  match lsb, msb with
  | x, 0 -> x
  | 0, x -> x
  | x, y -> if x<>y then raise Invalid_coding else x

let roundup n = if n mod 2 = 1 then n+1 else n

cstruct path_table_entry_lsb {
    uint8_t len;
    uint8_t ext_len;
    uint32_t location;
    uint16_t parent_dir;
} as little_endian

let unmarshal_one_pte_lsb v =
  let len = get_path_table_entry_lsb_len v in
  let ext_len = get_path_table_entry_lsb_ext_len v in
  let location = get_path_table_entry_lsb_location v in
  let parent = get_path_table_entry_lsb_parent_dir v in
  let name = Cstruct.to_string (Cstruct.sub v 8 len) in
  { pte_location=location; pte_parent=parent; pte_name=name }

let unmarshal_pte_lsb v =
  let size = Cstruct.len v in
  let rec inner n =
    if n >= size then [] else
      let v' = Cstruct.sub v n (size-n) in
      let pte = unmarshal_one_pte_lsb v' in
      let next = n + 8 + String.length pte.pte_name |> roundup in
      pte :: (inner next)
  in inner 0

cstruct susp {
    uint8_t signature[2];
    uint8_t len;
    uint8_t version;
} as little_endian

cstruct directory {
    uint8_t len;
    uint8_t ext_len;
    uint8_t location_lsb_msb[8];
    uint8_t data_len_lsb_msb[8];
    uint8_t date[7];
    uint8_t flags;
    uint8_t file_unit_size;
    uint8_t gap_size;
    uint8_t vol_seq_lsb_msb[4];
    uint8_t filename_length;
    uint8_t filename_start;
} as little_endian


let unmarshal_flags f =
  let fs = [
    0x1, Hidden;
    0x2, Directory;
    0x4, AssociatedFile;
    0x8, Format;
    0x10, Perms;
    (* 0x20 and 0x40 are reserved *)
    0x80, NotFinal ] in
  let rec test n acc =
    if n>0x80 then acc else
      let acc' =
        if n land f = n
        then (List.assoc n fs)::acc
        else acc
      in
      test (n*2) acc'
  in
  try
    test 1 []
  with Not_found ->
    failwith "Invalid flags entry"

let unmarshal_directory v =
  let len = get_directory_len v in
  Printf.printf "XXX len=%d\n%!" len;
  let ext_len = get_directory_ext_len v in
  let location = int32_of_lsb_msb (get_directory_location_lsb_msb v) in
  let data_len = int32_of_lsb_msb (get_directory_data_len_lsb_msb v) in
  let date = () in
  let flags_int = get_directory_flags v in
  let flags = unmarshal_flags flags_int in
  let file_unit_size = get_directory_file_unit_size v in
  let gap_size = get_directory_gap_size v in
  let vol_seq = int16_of_lsb_msb (get_directory_vol_seq_lsb_msb v) in
  let filename_len = get_directory_filename_length v in
  let filename = Cstruct.to_string (Cstruct.sub v 33 (filename_len)) in
  let rec unmarshal_susps n =
    Printf.printf "XXX n=%d\n%!" n;
    if n>=len-1 then [] else begin
      let susp_header = Cstruct.sub v n 4 in
      let signature = Cstruct.to_string (get_susp_signature susp_header) in
      Printf.printf "XXX signature=%s\n%!" signature;
      let len = get_susp_len susp_header in
      let version = get_susp_version susp_header in
      let data = Cstruct.sub v (n+4) (len-4) in
      { signature; version; data } :: unmarshal_susps (n+len)
    end
  in
  let susp = unmarshal_susps (roundup (33+filename_len)) in
  { len; ext_len; location; data_len; date; flags; file_unit_size; gap_size; vol_seq; filename; susp }

let maybe_unmarshal_directory v =
  let len = get_directory_len v in
  if len = 0 then None else begin
    Some (unmarshal_directory v)
  end

let print_directory d =
  let fields = [
    "len", string_of_int d.len;
    "ext_len", string_of_int d.ext_len;
    "location", Int32.to_string d.location;
    "data_len", Int32.to_string d.data_len;
    "flags", String.concat "," (List.map string_of_flag d.flags);
    "file_unit_size", string_of_int d.file_unit_size;
    "gap_size", string_of_int d.gap_size;
    "vol_seq", string_of_int d.vol_seq;
    "filename", d.filename ] in
  List.iter (fun (k,v) -> Printf.printf "%s: %s\n" k v) fields


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

type volume_descriptor =
  | Primary_volume_descriptor of primary_volume_descriptor
  | Boot_record
  | Supplementary_volume_descriptor
  | Volume_partition_descriptor
  | Volume_descriptor_set_terminator

let unmarshal_pvd pvd =
  let system_id = Cstruct.to_string (get_pvd_system_id pvd) in
  let volume_id = Cstruct.to_string (get_pvd_volume_id pvd) in
  let size = int32_of_lsb_msb (get_pvd_size_lsb_msb pvd) in
  let vol_set_size = int16_of_lsb_msb (get_pvd_vol_set_size_lsb_msb pvd) in
  let vol_seq_no = int16_of_lsb_msb (get_pvd_vol_seq_no_lsb_msb pvd) in
  let block_size = int16_of_lsb_msb (get_pvd_block_size_lsb_msb pvd) in
  let path_table_size = int32_of_lsb_msb (get_pvd_path_table_size_lsb_msb pvd) in
  let path_table_l_loc = get_pvd_l_path_table pvd in
  let opt_path_table_l_loc = get_pvd_opt_l_path_table pvd in
  let root_dir = unmarshal_directory (get_pvd_root_directory pvd) in
  let volume_set_id = Cstruct.to_string (get_pvd_volume_set_id pvd) in
  let publisher_id = Cstruct.to_string (get_pvd_publisher_id pvd) in
  let data_preparer_id = Cstruct.to_string (get_pvd_data_preparer_id pvd) in
  let app_id = Cstruct.to_string (get_pvd_app_id pvd) in
  let copyright = Cstruct.to_string (get_pvd_copyright pvd) in
  let abstract_file_id = Cstruct.to_string (get_pvd_abstract_file_id pvd) in
  let biblio_file_id = Cstruct.to_string (get_pvd_biblio_file_id pvd) in
  { system_id; volume_id; size; vol_set_size; vol_seq_no; block_size;
    path_table_size; path_table_l_loc = Some path_table_l_loc;
    opt_path_table_l_loc = Some opt_path_table_l_loc; path_table_m_loc = None;
    opt_path_table_m_loc = None; root_dir; volume_set_id; publisher_id;
    data_preparer_id; app_id; copyright; abstract_file_id; biblio_file_id }

let unmarshal_volume_descriptor (buf : Cstruct.t) =
  let ty = int_to_volume_descriptor_type (get_volume_descriptor_ty buf) in
  let id = Cstruct.to_string (get_volume_descriptor_id buf) in
  if id <> identifier_val then (`Error `Invalid_volume_descriptor_id) else
  match ty with
  | Some PRIMARY_VOLUME_DESCRIPTOR ->
    Printf.printf "Found PVD\n%!";
    begin
      try
        let pvd = get_volume_descriptor_data buf in
        `Ok (Primary_volume_descriptor (unmarshal_pvd pvd))
      with e ->
        `Error `Invalid_primary_volume_descriptor
    end
  | Some BOOT_RECORD ->
    Printf.printf "Found boot record\n%!";
    `Ok Boot_record
  | Some SUPPLEMENTARY_VOLUME_DESCRIPTOR ->
    `Ok Supplementary_volume_descriptor
  | Some VOLUME_PARTITION_DESCRIPTOR ->
    `Ok Volume_partition_descriptor
  | Some VOLUME_DESCRIPTOR_SET_TERMINATOR ->
    `Ok Volume_descriptor_set_terminator
  | None ->
    `Error `Unknown_volume_descriptor_type

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
  print_directory pvd.root_dir
