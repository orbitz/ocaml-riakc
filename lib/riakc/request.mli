open Core.Std

val ping         : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val client_id    : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val server_info  : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_buckets : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_keys    : string -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
