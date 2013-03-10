open Core.Std

module B = Protobuf.Builder

let wrap_request mc s =
  (* Add 1 for the mc *)
  let l = String.length s + 1 in
  let preamble_mc = String.create 5 in
  preamble_mc.[0] <- Char.of_int_exn ((l lsr 24) land 0xff);
  preamble_mc.[1] <- Char.of_int_exn ((l lsr 16) land 0xff);
  preamble_mc.[2] <- Char.of_int_exn ((l lsr 8) land 0xff);
  preamble_mc.[3] <- Char.of_int_exn (l land 0xff);
  preamble_mc.[4] <- mc;
  preamble_mc ^ s

let ping () =
  Ok (wrap_request '\x01' "")

let client_id () =
  Ok (wrap_request '\x03' "")

let server_info () =
  Ok (wrap_request '\x07' "")

let list_buckets () =
  Ok (wrap_request '\x0F' "")

let list_keys bucket () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes b 1 bucket >>= fun () ->
  Ok (wrap_request '\x11' (B.to_string b))
