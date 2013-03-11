open Core.Std

type t = { value : string }

let of_pb pb =
  { value = pb.Pb_robj.Robj.content.Pb_robj.Content.value }

let to_pb t  = failwith "nyi"

let value t = t.value
