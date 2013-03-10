open Core.Std
open Async.Std

let to_hex =
  String.concat_map
    ~f:(fun c ->
      sprintf "%X" (Char.to_int c))

let exec () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.client_id c       >>= fun client_id ->
  Riakc.Conn.close c           >>= fun () ->
  return (Ok client_id)

let eval () =
  exec () >>| function
    | Ok client_id -> begin
      printf "Client Id - %s\n" (to_hex client_id);
      shutdown 0
    end
    | Error _ -> begin
      printf "Failed\n";
      shutdown 1
    end

let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
