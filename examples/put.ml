open Core.Std
open Async.Std

let option_to_string =
  Option.value ~default:"<none>"

let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

let print_usermeta content =
  let module P = Riakc.Robj.Pair in
  List.iter
    ~f:(fun p ->
      printf "USERMETA: %s = %s\n" (P.key p) (option_to_string (P.value p)))
    (Riakc.Robj.Content.usermeta content)

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
      print_value content)

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  let b    = Sys.argv.(3) in
  let k    = Sys.argv.(4) in
  let v    = Sys.argv.(5) in
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c ->
      let module R = Riakc.Robj in
      let robj = R.set_content (R.Content.create v) (R.create []) in
      Riakc.Conn.put c ~b ~k robj)

let eval () =
  exec () >>| function
    | Ok (robj, _) -> begin
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
