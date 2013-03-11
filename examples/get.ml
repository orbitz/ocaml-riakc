open Core.Std
open Async.Std

let option_to_string = function
  | Some v -> v
  | None   -> "<none>"

let exec () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  let b    = Sys.argv.(3) in
  let k    = Sys.argv.(4) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.get c b k         >>= fun robj ->
  Riakc.Conn.close c           >>= fun () ->
  return (Ok robj)

let eval () =
  exec () >>| function
    | Ok robj -> begin
      printf "%s\n" (Riakc.Robj.value robj);
      shutdown 0
    end
    | Error _ -> begin
      printf "Failed\n";
      shutdown 1
    end

let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
