open Core.Std

type t =
  | Ping
  | Client_id of string
  | Server_info of (string option * string option)
  | Buckets of string list
  | Keys of string list

type error = [ `Bad_payload | `Incomplete_payload | Protobuf.Parser.error ]

type 'a cont = More of 'a | Done of 'a

val of_string    : string -> (t cont, [> error ]) Result.t
val parse_length : string -> (int, [> error ]) Result.t
