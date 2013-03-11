open Core.Std

type get = { bucket        : string
	   ; key           : string
	   ; r             : Int32.t option
	   ; pr            : Int32.t option
	   ; basic_quorum  : bool
	   ; notfound_ok   : bool
	   ; if_modified   : string option
	   ; head          : bool
	   ; deletedvclock : bool
	   }

val ping         : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val client_id    : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val server_info  : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_buckets : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_keys    : string -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val bucket_props : string -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val get          : get -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
