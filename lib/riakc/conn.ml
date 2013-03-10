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
  type error = [ `Bad_conn | Response.error ]

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


let rec read_str r pos s =
  Reader.read r ~pos s >>= function
    | `Ok l -> begin
      if (pos + l) <> String.length s then
	read_str r (pos + l) s
      else
	Deferred.return (Ok s)
    end
    | `Eof ->
      Deferred.return (Error `Bad_conn)

let parse_length preamble =
  Deferred.return (Response.parse_length preamble)

let parse_mc payload =
  Deferred.return (Response.parse_mc payload)

let read_payload r preamble =
  let open Deferred.Result.Monad_infix in
  parse_length preamble >>= fun resp_len ->
  let payload = String.create resp_len in
  read_str r 0 payload

let rec read_response mc r f =
  let open Deferred.Result.Monad_infix in
  let preamble = String.create 4 in
  read_str r 0 preamble   >>= fun _ ->
  read_payload r preamble >>= fun mc_payload ->
  parse_mc mc_payload     >>= function
    | (p_mc, payload) when p_mc = mc -> begin
      Deferred.return (f payload) >>= function
	| Response.More resp ->
	  read_response mc r f >>= fun more ->
	  Deferred.return (Ok (resp::more))
	| Response.Done resp ->
	  Deferred.return (Ok [resp])
    end
    | _ ->
      Deferred.return (Error `Bad_payload)

let do_request t mc g f =
  let open Deferred.Result.Monad_infix in
  Deferred.return (g ())    >>= fun request ->
  Writer.write t.w request;
  read_response mc t.r f

let connect ~host ~port =
  let connect () =
    Tcp.connect (Tcp.to_host_and_port host port)
  in
  Monitor.try_with connect >>| function
    | Ok (r, w) ->
      Ok { r; w }
    | Error _exn ->
      Error `Bad_conn

let close t =
  Writer.close t.w >>= fun () ->
  Deferred.return (Ok ())

let ping t =
  do_request
    t
    0x02
    Request.ping
    Response.ping
  >>| function
    | Ok [()] ->
      Ok ()
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let client_id t =
  do_request
    t
    0x04
    Request.client_id
    Response.client_id
  >>| function
    | Ok [client_id] ->
      Ok client_id
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let server_info t =
  do_request
    t
    0x08
    Request.server_info
    Response.server_info
  >>| function
    | Ok [(node, version)] ->
      Ok (node, version)
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let list_buckets t =
  do_request
    t
    0x10
    Request.list_buckets
    Response.list_buckets
  >>| function
    | Ok [buckets] ->
      Ok buckets
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let list_keys t bucket =
  do_request
    t
    0x12
    (Request.list_keys bucket)
    Response.list_keys
  >>| function
    | Ok keys ->
      Ok (List.concat keys)
    | Error err ->
      Error err

let get t ?(opts = []) ~b ~k =
  failwith "nyi"

let put t ?(opts = []) ~obj =
  failwith "nyi"
