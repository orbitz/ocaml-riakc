open Core.Std

type pair = (string * string option)
type keys = string list

val client_id    : string Protobuf.Parser.t
val server_info  : (string option * string option) Protobuf.Parser.t
val list_buckets : string list Protobuf.Parser.t
val list_keys    : (string list * bool) Protobuf.Parser.t
val bucket_props : (Int32.t option * bool option) Protobuf.Parser.t
val get          : (Pb_robj.Content.t list * string option * bool option) Protobuf.Parser.t
val put          : (Pb_robj.Content.t list * string option * string option) Protobuf.Parser.t
val index_search : (keys * pair list * string option * bool option) Protobuf.Parser.t
