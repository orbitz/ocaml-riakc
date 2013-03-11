open Core.Std

type t

val of_pb : Pb_robj.Robj.t -> t
val to_pb : t -> Pb_robj.Robj.t

val value : t -> string
