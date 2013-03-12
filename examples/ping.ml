open Core.Std
open Async.Std

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.ping c            >>= fun () ->
  Riakc.Conn.close c

let eval () =
  exec () >>| function
    | Ok () -> begin
      printf "pong\n";
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
