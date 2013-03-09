open Core.Std

type t =
  | Ping
  | Client_id of string
  | Server_info of (string * string)

type error = [ `Bad_payload | Protobuf.Parser.error ]

val of_bitstring : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
