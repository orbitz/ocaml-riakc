open Core.Std

type 'a t

module Usermeta : sig
  type t

  val create    : k:string -> v:string option -> t
  val key       : t -> string
  val value     : t -> string option
  val set_key   : string -> t -> t
  val set_value : string option -> t -> t
end

module Index : sig
  type idx = | String  of string
	     | Integer of int
	     | Bad_int of string
	     | Unknown of string

  type t

  val create    : k:string -> v:idx -> t
  val key       : t -> string
  val value     : t -> idx
  val set_key   : string -> t -> t
  val set_value : idx -> t -> t
end

module Link : sig
  type t

  val bucket : t -> string option
  val key    : t -> string option
  val tag    : t -> string option

  val set_bucket : string option -> t -> t
  val set_key    : string option -> t -> t
  val set_tag    : string option -> t -> t
end

module Content : sig
  type t

  val create               : string -> t

  val value                : t -> string
  val content_type         : t -> string option
  val charset              : t -> string option
  val content_encoding     : t -> string option
  val vtag                 : t -> string option
  val links                : t -> Link.t list
  val last_mod             : t -> Int32.t option
  val last_mod_usec        : t -> Int32.t option
  val usermeta             : t -> Usermeta.t list
  val indices              : t -> Index.t list
  val deleted              : t -> bool

  val set_value            : string -> t -> t
  val set_content_type     : string option -> t -> t
  val set_charset          : string option -> t -> t
  val set_content_encoding : string option -> t -> t
  val set_vtag             : string option -> t -> t
  val set_links            : Link.t list -> t -> t
  val set_last_mod         : Int32.t option -> t -> t
  val set_last_mod_usec    : Int32.t option -> t -> t
  val set_usermeta         : Usermeta.t list -> t -> t
  val set_indices          : Index.t list -> t -> t

  val to_pb : t -> Pb_robj.Content.t
  val of_pb : Pb_robj.Content.t -> t
end

val of_pb :
  Pb_robj.Content.t list ->
  string option ->
  bool option ->
  [ `Maybe_siblings ] t

val to_pb : 'a t -> (Pb_robj.Content.t list * string option)

val create       : Content.t -> [ `No_siblings ] t
val contents     : 'a t -> Content.t list
val content      : [ `No_siblings ] t -> Content.t
val vclock       : 'a t -> string option
val unchanged    : 'a t -> bool

val set_contents : Content.t list -> 'a t -> [ `Maybe_siblings ] t
val set_content  : Content.t -> 'a t -> [ `No_siblings ] t
val set_vclock   : string option -> 'a t -> 'a t
