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

