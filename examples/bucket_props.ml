open Core.Std
open Async.Std

let string_of_int_option = function
  | Some i -> sprintf "%d" i
  | None   -> "<none>"

let string_of_bool_option = function
  | Some true  -> "true"
  | Some false -> "false"
  | None       -> "<none>"

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  let b    = Sys.argv.(3) in
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c -> Riakc.Conn.bucket_props c b)

let eval () =
  exec () >>| function
    | Ok props -> begin
      printf "n_val: %s\nallow_mult: %s\n"
	(string_of_int_option props.Riakc.Response.n_val)
	(string_of_bool_option props.Riakc.Response.allow_mult);
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
