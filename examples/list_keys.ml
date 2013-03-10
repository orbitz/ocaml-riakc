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
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.list_keys c b     >>= fun keys ->
  Riakc.Conn.close c           >>= fun () ->
  return (Ok keys)

let eval () =
  exec () >>| function
    | Ok keys -> begin
      List.iter
	~f:(printf "%s\n")
	keys;
      shutdown 0
    end
    | Error _ -> begin
      printf "Failed\n";
      shutdown 1
    end

let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
