open Core.Std

type error = [ `Bad_payload | `Incomplete_payload | Protobuf.Parser.error ]

type 'a t = More of 'a | Done of 'a

type props = { n_val      : int option
	     ; allow_mult : bool option
	     }

val error        : Bitstring.bitstring -> (unit t, [> error ]) Result.t
val ping         : Bitstring.bitstring -> (unit t, [> error ]) Result.t
val client_id    : Bitstring.bitstring -> (string t, [> error ]) Result.t
val server_info  : Bitstring.bitstring -> ((string option * string option) t, [> error ]) Result.t
val list_buckets : Bitstring.bitstring -> (string list t, [> error ]) Result.t
val list_keys    : Bitstring.bitstring -> (string list t, [> error ]) Result.t
val bucket_props : Bitstring.bitstring -> (props t, [> error ]) Result.t

val parse_mc     : string -> ((int * Bitstring.bitstring), [> error ]) Result.t
val parse_length : string -> (int, [> error ]) Result.t
