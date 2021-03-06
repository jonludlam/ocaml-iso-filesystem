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

type t = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  hundredths : int;
  tz : int;
}
  
module Long = struct
  cstruct tx {
    uint8_t year[4];
    uint8_t month[2];
    uint8_t day[2];
    uint8_t hour[2];
    uint8_t minute[2];
    uint8_t second[2];
    uint8_t hundredths[2];
    uint8_t tz
  } as little_endian

  let unmarshal v =
    let year = int_of_string (Cstruct.to_string (get_tx_year v)) in
    let month = int_of_string (Cstruct.to_string (get_tx_month v)) in
    let day = int_of_string (Cstruct.to_string (get_tx_day v)) in
    let hour = int_of_string (Cstruct.to_string (get_tx_hour v)) in
    let minute = int_of_string (Cstruct.to_string (get_tx_minute v)) in
    let second = int_of_string (Cstruct.to_string (get_tx_second v)) in
    let hundredths = int_of_string (Cstruct.to_string (get_tx_hundredths v)) in
    let tz = get_tx_tz v in
    { year; month; day; hour; minute; second; hundredths; tz }

end

module Short = struct
  cstruct tx {
    uint8_t year;
    uint8_t month;
    uint8_t day;
    uint8_t hour;
    uint8_t minute;
    uint8_t second;
    uint8_t tz;
  } as little_endian

  let unmarshal v =
    let year = get_tx_year v in
    let month = get_tx_month v in
    let day = get_tx_day v in
    let hour = get_tx_hour v in
    let minute = get_tx_minute v in
    let second = get_tx_second v in
    let tz = get_tx_tz v in
    { year = year + 1900; month; day; hour; minute; second; hundredths=0; tz }
end
