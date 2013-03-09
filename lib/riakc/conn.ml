open Core.Std
open Async.Std

type t = { r : Reader.t
	 ; w : Writer.t
	 }

type error = [ `Bad_conn ]

module Quorum = struct
  type t =
    | One
    | All
    | Default
    | Quorum
    | N of int
end

module Get_opts = struct
  type error = [ `Bad_conn ]

  type t =
    | Timeout of int
    | Quorum_read of Quorum.t
end

module Put_opts = struct
  type error = [ `Bad_conn
	       | `Not_found
	       ]

  type t =
    | Timeout of int
    | Quorum_write of Quorum.t
end

let connect ~host ~port =
  let connect () =
    Tcp.connect (Tcp.to_host_and_port host port)
  in
  Monitor.try_with connect >>= function
    | Ok (r, w) ->
      Ok { r; w }
    | Error _exn ->
      Error `Bad_conn

let close t =
  Writer.close t.w

let ping t =
  failwith "nyi"

let get t ?(opts = []) ~b ~k =
  failwith "nyi"

let put t ?(opts = []) ~obj =
  failwith "nyi"
