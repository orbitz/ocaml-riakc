open Core.Std

type 'a t

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

  val create               : string -> t

  val value                : t -> string
  val content_type         : t -> string option
  val charset              : t -> string option
  val content_encoding     : t -> string option
  val vtag                 : t -> string option
  val last_mod             : t -> Int32.t option
  val last_mod_usec        : t -> Int32.t option
  val usermeta             : t -> Pair.t list
  val indexes              : t -> Pair.t list
  val deleted              : t -> bool

  val set_value            : string -> t -> t
  val set_content_type     : string option -> t -> t
  val set_charset          : string option -> t -> t
  val set_content_encoding : string option -> t -> t
  val set_vtag             : string option -> t -> t
  val set_last_mod         : Int32.t option -> t -> t
  val set_last_mod_usec    : Int32.t option -> t -> t
  val set_usermeta         : Pair.t list -> t -> t
  val set_indexes          : Pair.t list -> t -> t

  val to_pb : t -> Pb_robj.Content.t
  val of_pb : Pb_robj.Content.t ->  t
end

val of_pb :
  Pb_robj.Content.t list ->
  string option ->
  bool option ->
  [ `Maybe_siblings ] t

val to_pb : 'a t -> (Pb_robj.Content.t list * string option)

val create       : Content.t list -> [ `Maybe_siblings ] t
val contents     : 'a t -> Content.t list
val content      : [ `No_siblings ] t -> Content.t
val vclock       : 'a t -> string option
val unchanged    : 'a t -> bool

val set_contents : Content.t list -> 'a t -> [ `Maybe_siblings ] t
val set_content  : Content.t -> 'a t -> [ `No_siblings ] t
val set_vclock   : string option -> 'a t -> 'a t
