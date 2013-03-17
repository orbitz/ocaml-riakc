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

let bucket_props bucket () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes b 1 bucket >>= fun () ->
  Ok (wrap_request '\x13' (B.to_string b))

let get g () =
  let open Opts.Get in
  let basic_quorum  = Option.some_if g.basic_quorum true in
  let notfound_ok   = Option.some_if g.notfound_ok true in
  let head          = Option.some_if g.head true in
  let deletedvclock = Option.some_if g.deletedvclock true in
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes     b 1 g.bucket       >>= fun () ->
  B.bytes     b 2 g.key          >>= fun () ->
  B.int32_opt b 3 g.r            >>= fun () ->
  B.int32_opt b 4 g.pr           >>= fun () ->
  B.bool_opt  b 5 basic_quorum   >>= fun () ->
  B.bool_opt  b 6 notfound_ok    >>= fun () ->
  B.bytes_opt b 7 g.if_modified  >>= fun () ->
  B.bool_opt  b 8 head           >>= fun () ->
  B.bool_opt  b 9 deletedvclock  >>= fun () ->
  Ok (wrap_request '\x09' (B.to_string b))

let put p () =
  let open Opts.Put in
  let content         = Robj.Content.to_pb p.content in
  let return_body     = Option.some_if p.return_body true in
  let if_not_modified = Option.some_if p.if_not_modified true in
  let if_none_match   = Option.some_if p.if_none_match true in
  let return_head     = Option.some_if p.return_head true in
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes     b  1 p.bucket                      >>= fun () ->
  B.bytes_opt b  2 p.key                         >>= fun () ->
  B.bytes_opt b  3 p.vclock                      >>= fun () ->
  B.embd_msg  b  4 content Pb_robj.Content.build >>= fun () ->
  B.int32_opt b  5 p.w                           >>= fun () ->
  B.int32_opt b  6 p.dw                          >>= fun () ->
  B.bool_opt  b  7 return_body                   >>= fun () ->
  B.int32_opt b  8 p.pw                          >>= fun () ->
  B.bool_opt  b  9 if_not_modified               >>= fun () ->
  B.bool_opt  b 10 if_none_match                 >>= fun () ->
  B.bool_opt  b 11 return_head                   >>= fun () ->
  Ok (wrap_request '\x0B' (B.to_string b))

let delete d () =
  let open Opts.Delete in
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes     b 1 d.bucket >>= fun () ->
  B.bytes     b 2 d.key    >>= fun () ->
  B.int32_opt b 3 d.rw     >>= fun () ->
  B.bytes_opt b 4 d.vclock >>= fun () ->
  B.int32_opt b 5 d.r      >>= fun () ->
  B.int32_opt b 6 d.w      >>= fun () ->
  B.int32_opt b 7 d.pr     >>= fun () ->
  B.int32_opt b 8 d.pw     >>= fun () ->
  B.int32_opt b 9 d.dw     >>= fun () ->
  Ok (wrap_request '\x0D' (B.to_string b))
