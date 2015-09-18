open Result

type flag = | Hidden | Directory | AssociatedFile | Format | Perms | NotFinal

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
  susp : Susp.t list;
}

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
      let next = n + 8 + String.length pte.pte_name |> Multibyte.roundup in
      pte :: (inner next)
  in inner 0

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
  let ext_len = get_directory_ext_len v in
  let location = Multibyte.int32_of_lsb_msb (get_directory_location_lsb_msb v) in
  let data_len = Multibyte.int32_of_lsb_msb (get_directory_data_len_lsb_msb v) in
  let date = () in
  let flags_int = get_directory_flags v in
  let flags = unmarshal_flags flags_int in
  let file_unit_size = get_directory_file_unit_size v in
  let gap_size = get_directory_gap_size v in
  let vol_seq = Multibyte.int16_of_lsb_msb (get_directory_vol_seq_lsb_msb v) in
  let filename_len = get_directory_filename_length v in
  let filename = Cstruct.to_string (Cstruct.sub v 33 (filename_len)) in
  let susp_start = Multibyte.roundup (33+filename_len) in
  Susp.unmarshal (Cstruct.sub v susp_start (len - susp_start))
  >>= fun susp ->
  `Ok { len; ext_len; location; data_len; date; flags; file_unit_size; gap_size; vol_seq; filename; susp }

let maybe_unmarshal_directory v =
  let len = get_directory_len v in
  if len = 0 then `Ok None else begin
    unmarshal_directory v
    >>= fun dir ->
    `Ok (Some dir)
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

let get_filename d =
  let filename = List.fold_left
      (fun acc s -> match s with | Susp.NM nm -> nm.Susp.Nm.filename | _ -> acc)
      d.filename d.susp in
  if List.mem Directory d.flags
  then begin
    match d.filename with
    | "\000" -> "."
    | "\001" -> ".."
    | x -> filename
  end else filename
