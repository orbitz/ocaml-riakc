open Core.Std

module Quorum = struct
  type t =
    | One
    | All
    | Default
    | Quorum
    | N of int

  let one     = Int32.of_int_exn (-2)
  let quorum  = Int32.of_int_exn (-3)
  let all     = Int32.of_int_exn (-4)
  let default = Int32.of_int_exn (-5)

  let to_int32 = function
    | N n when n > 1000 ->
      (* Arbitrary and cheap, but n should always be small *)
      failwith "to_int32 - n too large"
    | N n ->
      Int32.of_int_exn n
    | One ->
      one
    | All ->
      all
    | Default ->
      default
    | Quorum ->
      quorum

  let of_int32 = function
    | n when Int32.equal n one ->
      One
    | n when Int32.equal n all ->
      All
    | n when Int32.equal n default ->
      Default
    | n when Int32.equal n quorum ->
      Quorum
    | n -> begin
      match Int32.to_int n with
	| Some n ->
	  N n
	| None ->
	  failwith "of_int32 - n too large"
    end


end

module Get = struct
  type error = [ `Bad_conn | `Notfound | Response.error ]

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

  let get_of_opts opts ~b ~k =
    let g = { bucket        = b
	    ; key           = k
	    ; r             = None
	    ; pr            = None
	    ; basic_quorum  = false
	    ; notfound_ok   = false
	    ; if_modified   = None
	    ; head          = false
	    ; deletedvclock = false
	    }
    in
    List.fold_left
      ~f:(fun g -> function
	| Timeout _ ->
	  g
	| R n ->
	  { g with r = Some (Quorum.to_int32 n) }
	| Pr n ->
	  { g with pr = Some (Quorum.to_int32 n) }
	| If_modified s ->
	  { g with if_modified = Some s }
	| Basic_quorum ->
	  { g with basic_quorum = true }
	| Notfound_ok ->
	  { g with notfound_ok = true }
	| Head ->
	  { g with head = true }
	| Deletedvclock ->
	  { g with deletedvclock = true })
      ~init:g
      opts
end

module Put = struct
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

  let put_of_opts opts ~b ~k robj =
    let p = { bucket          = b
	    ; key             = k
	    ; vclock          = Robj.vclock robj
	    ; content         = Robj.content robj
	    ; w               = None
	    ; dw              = None
	    ; pw              = None
	    ; return_body     = false
	    ; if_not_modified = false
	    ; if_none_match   = false
	    ; return_head     = false
	    }
    in
    List.fold_left
      ~f:(fun p -> function
	| Timeout _ ->
	  p
	| W n ->
	  { p with w = Some (Quorum.to_int32 n) }
	| Dw n ->
	  { p with dw = Some (Quorum.to_int32 n) }
	| Pw n ->
	  { p with pw = Some (Quorum.to_int32 n) }
	| Return_body ->
	  { p with return_body = true }
	| If_not_modified ->
	  { p with if_not_modified = true }
	| If_none_match ->
	  { p with if_none_match = true }
	| Return_head ->
	  { p with return_head = true })
      ~init:p
      opts
end

module Delete = struct
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

  let delete_of_opts opts ~b ~k =
    let d = { bucket = b
	    ; key    = k
	    ; rw     = None
	    ; vclock = None
	    ; r      = None
	    ; w      = None
	    ; pr     = None
	    ; pw     = None
	    ; dw     = None
	    }
    in
    List.fold_left
      ~f:(fun d -> function
	| Timeout _ ->
	  d
	| Rw n ->
	  { d with rw = Some (Quorum.to_int32 n) }
	| R n ->
	  { d with w = Some (Quorum.to_int32 n) }
	| W n ->
	  { d with dw = Some (Quorum.to_int32 n) }
	| Pr n ->
	  { d with pr = Some (Quorum.to_int32 n) }
	| Pw n ->
	  { d with pw = Some (Quorum.to_int32 n) }
	| Dw n ->
	  { d with dw = Some (Quorum.to_int32 n) })
      ~init:d
      opts
end
