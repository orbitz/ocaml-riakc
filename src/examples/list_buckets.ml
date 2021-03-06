open Core.Std
open Async.Std

let option_to_string = function
  | Some v -> v
  | None   -> "<none>"

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.with_conn
    ~host
    ~port
    Riakc.Conn.list_buckets

let eval () =
  exec () >>| function
    | Ok buckets -> begin
      List.iter
	~f:(printf "%s\n")
	buckets;
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
