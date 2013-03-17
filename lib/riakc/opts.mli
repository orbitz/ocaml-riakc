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
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout of int
    | W       of Quorum.t
    | Dw      of Quorum.t
    | Pw      of Quorum.t
    | Return_body
    | If_not_modified
    | If_none_match
    | Return_head

  type put = { bucket          : string
	     ; key             : string option
	     ; vclock          : string option
	     ; content         : Robj.Content.t
	     ; w               : Int32.t option
	     ; dw              : Int32.t option
	     ; pw              : Int32.t option
	     ; return_body     : bool
	     ; if_not_modified : bool
	     ; if_none_match   : bool
	     ; return_head     : bool
	     }

  val put_of_opts : t list -> b:string -> k:string option -> [ `No_siblings ] Robj.t -> put
end

module Delete : sig
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout of int
    | Rw      of Quorum.t
    | R       of Quorum.t
    | W       of Quorum.t
    | Pr      of Quorum.t
    | Pw      of Quorum.t
    | Dw      of Quorum.t

  type delete = { bucket : string
		; key    : string
		; rw     : Int32.t option
		; vclock : string option
		; r      : Int32.t option
		; w      : Int32.t option
		; pr     : Int32.t option
		; pw     : Int32.t option
		; dw     : Int32.t option
		}

  val delete_of_opts : t list -> b:string -> k:string -> delete
end
