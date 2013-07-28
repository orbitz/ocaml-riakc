open Core.Std
open Async.Std

let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

let fail s =
  printf "%s\n" s;
  shutdown 1

let parse_qt idx =
  let module Is = Riakc.Opts.Index_search in
  match Sys.argv.(idx) with
    | "eq:int" ->
      Is.Query.eq_int (Int.of_string (Sys.argv.(idx + 1)))
    | "eq:bin" ->
      Is.Query.eq_string Sys.argv.(idx + 1)
    | "range:int" ->
      Is.Query.range_int
	~min:(Int.of_string (Sys.argv.(idx + 1)))
	~max:(Int.of_string (Sys.argv.(idx + 2)))
	~return_terms:false
    | "range:bin" ->
      Is.Query.range_string
	~min:Sys.argv.(idx + 1)
	~max:Sys.argv.(idx + 2)
	~return_terms:false
    | search ->
      failwith ("Unknown search: " ^ search)

let print_keys results =
  List.iter
    ~f:(printf "%s\n")
    results.Riakc.Response.keys

let exec () =
  let host       = Sys.argv.(1) in
  let port       = Int.of_string Sys.argv.(2) in
  let b          = Sys.argv.(3) in
  let index      = Sys.argv.(4) in
  let qt         = parse_qt 5   in
  let consumer r = print_keys r; Deferred.return () in
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c ->
      Riakc.Conn.index_search_stream
	c
	~b
	~index
	~consumer
	qt)

let eval () =
  exec () >>| function
    | Ok ()                     -> shutdown 0
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
