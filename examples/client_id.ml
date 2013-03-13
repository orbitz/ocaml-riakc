open Core.Std
open Async.Std

let to_hex =
  String.concat_map
    ~f:(fun c ->
      sprintf "%X" (Char.to_int c))

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.with_conn
    ~host
    ~port
    Riakc.Conn.client_id

let eval () =
  exec () >>| function
    | Ok client_id -> begin
      printf "Client Id - %s\n" (to_hex client_id);
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
