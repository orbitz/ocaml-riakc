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

let bucket_props =
  let props =
    P.int32_opt 1 >>= fun n_val ->
    P.bool_opt  2 >>= fun allow_mult ->
    P.return (n_val, allow_mult)
  in
  P.embd_msg 1 props >>= P.return

let get = Pb_robj.Robj.parse
