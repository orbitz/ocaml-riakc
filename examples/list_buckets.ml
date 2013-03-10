open Core.Std
open Async.Std

let option_to_string = function
  | Some v -> v
  | None   -> "<none>"

let ping () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.list_buckets c    >>= fun buckets ->
  Riakc.Conn.close c           >>= fun () ->
  return (Ok buckets)

let perform_ping () =
  ping () >>| function
    | Ok buckets -> begin
      List.iter
	~f:(printf "%s\n")
	buckets;
      shutdown 0
    end
    | Error _ -> begin
      printf "Failed\n";
      shutdown 1
    end

let () =
  ignore (perform_ping ());
  never_returns (Scheduler.go ())
