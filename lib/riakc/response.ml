module Old_int32 = Int32
module Old_char = Char
module Old_string = String

open Core.Std

module P = Protobuf.Parser

type error = [ `Bad_payload | `Incomplete_payload | P.error ]

type 'a t = More of 'a | Done of 'a

type props = { n_val      : int option
	     ; allow_mult : bool option
	     }

let run payload f =
  let open Result.Monad_infix in
  P.State.create payload >>= fun s ->
  P.run f s              >>= fun (r, _) ->
  Ok r

let error payload =
  failwith "nyi"

let ping payload =
  match Bitstring.string_of_bitstring payload with
    | "" ->
      Ok (Done ())
    | _ ->
      Error `Bad_payload

let client_id payload =
  let open Result.Monad_infix in
  run payload Pb_response.client_id >>= fun client_id ->
  Ok (Done client_id)

let server_info payload =
  let open Result.Monad_infix in
  run payload Pb_response.server_info >>= fun server_info ->
  Ok (Done server_info)

let list_buckets payload =
  let open Result.Monad_infix in
  run payload Pb_response.list_buckets >>= fun buckets ->
  Ok (Done buckets)

let list_keys payload =
  let open Result.Monad_infix in
  run payload Pb_response.list_keys >>= function
    | (keys, false) ->
      Ok (More keys)
    | (keys, true) ->
      Ok (Done keys)

let bucket_props payload =
  let open Result.Monad_infix in
  run payload Pb_response.bucket_props >>= fun (n_val, allow_mult) ->
  match n_val with
    | Some n_val32 -> begin
      match Int32.to_int n_val32 with
	| Some n_val ->
	  Ok (Done { n_val = Some n_val; allow_mult })
	| None ->
	  Error `Overflow
    end
    | None ->
      Ok (Done { n_val = None; allow_mult })

let parse_mc s =
  let bits = Bitstring.bitstring_of_string s in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  bitmatch bits with
    | { mc      : 8
      ; payload : -1 : bitstring
      } ->
      Ok (mc, payload)
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
