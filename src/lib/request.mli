open Core.Std

val ping         : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val client_id    : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val server_info  : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val bucket_props : string -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_buckets : unit -> (string, [> Protobuf.Builder.error ]) Result.t
val list_keys    : string -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val get          : Opts.Get.get -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val put          : Opts.Put.put -> unit -> (string, [> Protobuf.Builder.error ]) Result.t
val delete       : Opts.Delete.delete -> unit -> (string, [> Protobuf.Builder.error ]) Result.t

val index_search :
  stream:bool ->
  Opts.Index_search.index_search ->
  unit ->
  (string, [> Protobuf.Builder.error ]) Result.t

