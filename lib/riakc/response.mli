open Core.Std

type error = [ `Bad_payload | `Incomplete_payload | Protobuf.Parser.error ]

type 'a t = More of 'a | Done of 'a

type props = { n_val      : int option
	     ; allow_mult : bool option
	     }

val error        : string -> (unit t, [> error ]) Result.t
val ping         : string -> (unit t, [> error ]) Result.t
val client_id    : string -> (string t, [> error ]) Result.t
val server_info  : string -> ((string option * string option) t, [> error ]) Result.t
val list_buckets : string -> (string list t, [> error ]) Result.t
val list_keys    : string -> (string list t, [> error ]) Result.t
val bucket_props : string -> (props t, [> error ]) Result.t
val get          : string -> (Robj.t t, [> error ]) Result.t

val parse_length : string -> (int, [> error ]) Result.t
