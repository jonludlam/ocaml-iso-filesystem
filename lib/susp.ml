

type entry = {
  signature : string;
  version : int;
  data : Cstruct.t;
}

cstruct susp {
    uint8_t signature[2];
    uint8_t len;
    uint8_t version;
} as little_endian


let unmarshal v =
  let len = Cstruct.len v in
  let rec inner n =
    Printf.printf "XXX n=%d\n%!" n;
    if n>=len-1 then [] else begin
      let susp_header = Cstruct.sub v n 4 in
      let signature = Cstruct.to_string (get_susp_signature susp_header) in
      Printf.printf "XXX signature=%s\n%!" signature;
      let len = get_susp_len susp_header in
      let version = get_susp_version susp_header in
      let data = Cstruct.sub v (n+4) (len-4) in
      { signature; version; data } :: inner (n+len)
    end
  in inner 0
