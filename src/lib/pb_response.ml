open Core.Std

module P = Protobuf.Parser

open P.Monad_infix

type pair = (string * string option)
type keys = string list

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

let get =
  P.embd_msg_rep 1 Pb_robj.Content.parse >>= fun contents ->
  P.bytes_opt    2                       >>= fun vclock ->
  P.bool_opt     3                       >>= fun unchanged ->
  P.return (contents, vclock, unchanged)

let put =
  P.embd_msg_rep 1 Pb_robj.Content.parse >>= fun contents ->
  P.bytes_opt    2                       >>= fun vclock ->
  P.bytes_opt    3                       >>= fun key ->
  P.return (contents, vclock, key)

let pair =
  P.bytes     1 >>= fun key ->
  P.bytes_opt 2 >>= fun value ->
  P.return (key, value)

let index_search =
  P.bytes_rep    1      >>= fun keys ->
  P.embd_msg_rep 2 pair >>= fun results ->
  P.bytes_opt    3      >>= fun cont ->
  P.bool_opt     4      >>= fun d ->
  P.return (keys, results, cont, d)
