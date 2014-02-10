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

module Index_search : sig
  type error = [ `Bad_conn | Response.error ]

  module Query : sig
    type 'a range = { min          : 'a
		    ; max          : 'a
		    ; return_terms : bool
		    }

    type t =
      | Eq_string    of string
      | Eq_int       of int
      | Range_string of string range
      | Range_int    of int range

    val eq_string    : string -> t
    val eq_int       : int -> t
    val range_string : min:string -> max:string -> return_terms:bool -> t
    val range_int    : min:int    -> max:int    -> return_terms:bool -> t
  end

  module Kontinuation : sig
    type t

    val of_string : string -> t
    val to_string : t -> string
  end

  type t =
    | Timeout      of int
    | Max_results  of Int32.t
    | Continuation of Kontinuation.t

  type index_search = { bucket       : string
		      ; index        : string
		      ; query_type   : Query.t
		      ; max_results  : Int32.t option
		      ; continuation : Kontinuation.t option
		      ; timeout      : int option
		      }

  val index_search_of_opts :
    t list ->
    b:string ->
    index:string ->
    query_type:Query.t ->
    index_search
end
