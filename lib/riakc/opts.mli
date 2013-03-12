module Quorum : sig
  type t =
    | One
    | All
    | Default
    | Quorum
    | N of int

  val to_int32 : t -> Int32.t
  val of_int32 : Int32.t -> t
end

module Get : sig
  type error =[ `Bad_conn | `Notfound | Response.error ]

  type t =
    | Timeout     of int
    | R           of Quorum.t
    | Pr          of Quorum.t
    | If_modified of string
    | Basic_quorum
    | Notfound_ok
    | Head
    | Deletedvclock

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

  val get_of_opts : t list -> b:string -> k:string -> get

end

module Put : sig
  type error = [ `Bad_conn
	       | `Not_found
	       ]

  type t =
    | Timeout of int
    | Quorum_write of Quorum.t
end
