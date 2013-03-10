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

let read_payload r preamble =
  let open Deferred.Result.Monad_infix in
  parse_length preamble >>= fun resp_len ->
  let payload = String.create resp_len in
  read_str r 0 payload

let do_request t f =
  let open Deferred.Result.Monad_infix in
  let preamble = String.create 4 in
  Deferred.return (f ())    >>= fun request ->
  Writer.write t.w request;
  read_str t.r 0 preamble   >>= fun _ ->
  read_payload t.r preamble >>= fun payload ->
  Deferred.return (Response.of_string payload)

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
  do_request t Request.ping >>| function
    | Ok Response.Ping ->
      Ok ()
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let client_id t =
  do_request t Request.client_id >>| function
    | Ok (Response.Client_id client_id) ->
      Ok client_id
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let server_info t =
  do_request t Request.server_info >>| function
    | Ok (Response.Server_info (node, version)) ->
      Ok (node, version)
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let list_buckets t =
  do_request t Request.list_buckets >>| function
    | Ok (Response.Buckets buckets) ->
      Ok buckets
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let list_keys t bucket =
  do_request t (Request.list_keys bucket) >>| function
    | Ok (Response.Keys (keys, _)) ->
      Ok keys
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err

let get t ?(opts = []) ~b ~k =
  failwith "nyi"

let put t ?(opts = []) ~obj =
  failwith "nyi"
