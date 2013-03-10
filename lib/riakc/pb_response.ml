open Core.Std

module P = Protobuf.Parser

open P.Monad_infix

let client_id =
  P.bytes 1 >>= P.return

let server_info =
  P.bytes_opt 1 >>= fun node ->
  P.bytes_opt 2 >>= fun server ->
  P.return (node, server)

let list_buckets =
  P.bytes_rep 1 >>= P.return

let list_keys =
  P.bytes_rep 1 >>= fun keys ->
  P.bool_opt  2 >>= function
    | Some true ->
      P.return (keys, true)
    | _ ->
      P.return (keys, false)

