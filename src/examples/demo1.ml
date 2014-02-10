(*
 * This example is valid for version 2.0.0, and possibly later
 *)
open Core.Std
open Async.Std

(*
 * Take a string of bytes and convert them to hex string
 * representation
 *)
let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

(*
 * An Robj can have multiple values in it, each one with its
 * own content type, encoding, and value.  This just prints
 * the value, which is a string blob
 *)
let print_contents contents =
  List.iter
    ~f:(fun content ->
      let module C = Riakc.Robj.Content in
      printf "VALUE: %s\n" (C.value content))
    contents

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  (*
   * [with_conn] is a little helper function that will
   * establish a connection, run a function on the connection
   * and tear it down when done
   *)
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c ->
      let module R = Riakc.Robj in
      let content  = R.Content.create "some random data" in
      let robj     = R.create content in
      (*
       * Put takes a bucket, a key, and an optional list of
       * options.  In this case we are setting the
       * [Return_body] option which returns what the key
       * looks like after the put.  It is possible that
       * siblings were created.
       *)
      Riakc.Conn.put
	c
	~b:"test_bucket"
	~k:"test_key"
	~opts:[Riakc.Opts.Put.Return_body]
	robj)

let eval () =
  exec () >>| function
    | Ok (robj, key) -> begin
      (*
       * [put] returns a [Riakc.Robj.t] and a [string
       * option], which is the key if Riak had to generate
       * it
       *)
      let module R = Riakc.Robj in
      (*
       * Extract the vclock, if it exists, and convert it to
       * to something printable
       *)
      let vclock =
	Option.value
	  ~default:"<none>"
	  (Option.map ~f:hex_of_string (R.vclock robj))
      in
      let key = Option.value ~default:"<none>" key in
      printf "KEY: %s\n" key;
      printf "VCLOCK: %s\n" vclock;
      print_contents (R.contents robj);
      shutdown 0
    end
    (*
     * These are the various errors that can be returned.
     * Many of then come directly from the ProtoBuf layer
     * since there aren't really any more semantics to apply
     * to the data if it matches the PB frame.
     *)
    | Error `Bad_conn           -> fail "Bad_conn"
    | Error `Bad_payload        -> fail "Bad_payload"
    | Error `Incomplete_payload -> fail "Incomplete_payload"
    | Error `Notfound           -> fail "Notfound"
    | Error `Incomplete         -> fail "Incomplete"
    | Error `Overflow           -> fail "Overflow"
    | Error `Unknown_type       -> fail "Unknown_type"
    | Error `Wrong_type         -> fail "Wrong_type"

let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
