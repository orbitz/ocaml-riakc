open Core.Std

module P = Protobuf.Parser

open P.Monad_infix

let client_id =
  P.bytes 1 >>= P.return

let server_info =
  P.bytes_opt 1 >>= fun node ->
  P.bytes_opt 2 >>= fun server ->
  P.return (node, server)
