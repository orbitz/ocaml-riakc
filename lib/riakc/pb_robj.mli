open Core.Std

module Link : sig
  type t = { bucket : string option
	   ; key    : string option
	   ; tag    : string option
	   }

  val parse : t Protobuf.Parser.t
end

module Pair : sig
  type t = { key   : string
	   ; value : string option
	   }

  val parse : t Protobuf.Parser.t
end

module Content : sig
  type t = { value            : string
	   ; content_type     : string option
	   ; charset          : string option
	   ; content_encoding : string option
	   ; vtag             : string option
	   ; links            : Link.t list
	   ; last_mod         : Int32.t option
	   ; last_mod_usec    : Int32.t option
	   ; usermeta         : Pair.t list
	   ; indexes          : Pair.t list
	   ; deleted          : bool option
	   }

  val parse : t Protobuf.Parser.t
end

module Robj : sig
  type t = { contents  : Content.t list
	   ; vclock    : string option
	   ; unchanged : bool option
	   }

  val parse : t Protobuf.Parser.t
end
