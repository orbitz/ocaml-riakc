module Old_int32 = Int32
module Old_char = Char
module Old_string = String

open Core.Std

type t =
  | Ping
  | Client_id of string
  | Server_info of (string * string)

type error = [ `Bad_payload | `Incomplete_payload | Protobuf.Parser.error ]

let parse_error payload =
  failwith "nyi"

let parse_ping payload =
  match Bitstring.string_of_bitstring payload with
    | "" ->
      Ok Ping
    | _ ->
      Error `Bad_payload

let parse_client_id payload =
  failwith "nyi"

let parse_server_info payload =
  failwith "nyi"

let message_code =
  Int.Map.of_alist_exn
    [ (0, parse_error)
    ; (2, parse_ping)
    ; (4, parse_client_id)
    ; (8, parse_server_info)
    ]

let find_mc mc =
  match Int.Map.find message_code mc with
    | Some prsr ->
      Ok prsr
    | None ->
      Error `Wrong_type

let parse_resp mc payload =
  let open Result.Monad_infix in
  find_mc mc >>= fun prsr ->
  prsr payload

let of_string s =
  let bits = Bitstring.bitstring_of_string s in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  bitmatch bits with
    | { mc      : 8
      ; payload : -1 : bitstring
      } ->
      parse_resp mc payload >>= fun resp ->
      Ok resp
    | { _ } ->
      Error `Incomplete_payload

let parse_length s =
  let bits = Bitstring.bitstring_of_string s in
  let to_int = Int32.to_int in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  bitmatch bits with
    | { len : 32 : bigendian } -> begin
      match to_int len with
	| Some n ->
	  Ok n
	| None ->
	  Error `Overflow
    end
    | { _ } ->
      Error `Incomplete
