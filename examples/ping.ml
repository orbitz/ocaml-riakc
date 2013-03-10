open Core.Std
open Async.Std

let ping () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.ping c            >>= fun () ->
  Riakc.Conn.close c

let perform_ping () =
  ping () >>| function
    | Ok () -> begin
      printf "pong\n";
      shutdown 0
    end
    | Error _ -> begin
      printf "pang\n";
      shutdown 1
    end

let () =
  ignore (perform_ping ());
  never_returns (Scheduler.go ())
