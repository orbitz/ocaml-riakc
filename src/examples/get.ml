open Core.Std
open Async.Std

let option_to_string =
  Option.value ~default:"<none>"

let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

let print_usermeta content =
  let module U = Riakc.Robj.Usermeta in
  List.iter
    ~f:(fun u ->
      printf "USERMETA: %s = %s\n" (U.key u) (option_to_string (U.value u)))
    (Riakc.Robj.Content.usermeta content)

let print_indices content =
  let module I = Riakc.Robj.Index in
  let index_value_to_string = function
    | I.String s  -> "String " ^ s
    | I.Integer i -> "Integer " ^ (Int.to_string i)
    | I.Bad_int s -> "Bad_int " ^ s
    | I.Unknown s -> "Unknown " ^ s
  in
  List.iter
    ~f:(fun i ->
      printf "INDEX: %s = %s\n" (I.key i) (index_value_to_string (I.value i)))
    (Riakc.Robj.Content.indices content)

let print_value content =
  let value = Riakc.Robj.Content.value content in
  List.iter
    ~f:(printf "CONTENT: %s\n")
    (String.split ~on:'\n' value)

let print_contents =
  List.iter
    ~f:(fun content ->
      let module C = Riakc.Robj.Content in
      printf "CONTENT_TYPE: %s\n" (option_to_string (C.content_type content));
      printf "CHARSET: %s\n" (option_to_string (C.charset content));
      printf "CONTENT_ENCODING: %s\n" (option_to_string (C.content_encoding content));
      print_usermeta content;
      print_indices content;
      print_value content)

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  let b    = Sys.argv.(3) in
  let k    = Sys.argv.(4) in
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c -> Riakc.Conn.get c ~b k)

let eval () =
  exec () >>| function
    | Ok robj -> begin
      let module R = Riakc.Robj in
      let vclock =
	match R.vclock robj with
	  | Some v ->
	    hex_of_string v
	  | None ->
	    "<none>"
      in
      printf "VCLOCK: %s\n" vclock;
      print_contents (R.contents robj);
      shutdown 0
    end
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
