open Core.Std

type t

module Pair : sig
  type t

  val key   : t -> string
  val value : t -> string option
end

module Link : sig
  type t
end

module Content : sig
  type t

  val value            : t -> string
  val content_type     : t -> string option
  val charset          : t -> string option
  val content_encoding : t -> string option
  val vtag             : t -> string option
  val last_mod         : t -> Int32.t option
  val last_mod_usec    : t -> Int32.t option
  val usermeta         : t -> Pair.t list
  val indexes          : t -> Pair.t list
  val deleted          : t -> bool
end

val of_pb : Pb_robj.Robj.t -> t
val to_pb : t -> Pb_robj.Robj.t

val contents  : t -> Content.t list
val vclock    : t -> string option
val unchanged : t -> bool
