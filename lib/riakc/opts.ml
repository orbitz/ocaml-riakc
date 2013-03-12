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
  type error = [ `Bad_conn | Response.error ]

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
  type error = [ `Bad_conn
	       | `Not_found
	       ]

  type t =
    | Timeout of int
    | Quorum_write of Quorum.t
end
