module Old_int32 = Int32
module Old_char = Char
module Old_string = String

open Core.Std

module P = Protobuf.Parser

type t =
  | Ping
  | Client_id of string
  | Server_info of (string option * string option)
  | Buckets of string list
  | Keys of (string list * bool)

type error = [ `Bad_payload | `Incomplete_payload | P.error ]

let run payload f =
  let open Result.Monad_infix in
  P.State.create payload >>= fun s ->
  P.run f s              >>= fun (r, _) ->
  Ok r

let parse_error payload =
  failwith "nyi"

let parse_ping payload =
  match Bitstring.string_of_bitstring payload with
    | "" ->
      Ok Ping
    | _ ->
      Error `Bad_payload

let parse_client_id payload =
  let open Result.Monad_infix in
  run payload Pb_response.client_id >>= fun client_id ->
  Ok (Client_id client_id)

let parse_server_info payload =
  let open Result.Monad_infix in
  run payload Pb_response.server_info >>= fun server_info ->
  Ok (Server_info server_info)

let parse_list_buckets payload =
  let open Result.Monad_infix in
  run payload Pb_response.list_buckets >>= fun buckets ->
  Ok (Buckets buckets)

let parse_list_keys payload =
  let open Result.Monad_infix in
  run payload Pb_response.list_keys >>= fun (keys, d) ->
  Ok (Keys (keys, d))

let message_code =
  Int.Map.of_alist_exn
    [ (0x00, parse_error)
    ; (0x02, parse_ping)
    ; (0x04, parse_client_id)
    ; (0x08, parse_server_info)
    ; (0x10, parse_list_buckets)
    ; (0x12, parse_list_keys)
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
