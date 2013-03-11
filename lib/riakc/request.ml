open Core.Std

module B = Protobuf.Builder

type get = { bucket        : string
	   ; key           : string
	   ; r             : Int32.t option
	   ; pr            : Int32.t option
	   ; basic_quorum  : bool
	   ; notfound_ok   : bool
	   ; if_modified   : string option
	   ; head          : bool
	   ; deletedvclock : bool
	   }

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
