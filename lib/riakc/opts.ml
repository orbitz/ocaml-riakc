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
end

module Put = struct
  type error = [ `Bad_conn
	       | `Not_found
	       ]

  type t =
    | Timeout of int
    | Quorum_write of Quorum.t
end
