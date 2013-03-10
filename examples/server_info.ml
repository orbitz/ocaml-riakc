open Core.Std
open Async.Std

let to_hex =
  String.concat_map
    ~f:(fun c ->
      sprintf "%X" (Char.to_int c))

let option_to_hex = function
  | Some v -> to_hex v
  | None   -> "<none>"

let ping () =
  let open Deferred.Result.Monad_infix in
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  Riakc.Conn.connect host port >>= fun c ->
  Riakc.Conn.server_info c     >>= fun server_info ->
  Riakc.Conn.close c           >>= fun () ->
  return (Ok server_info)

let perform_ping () =
  ping () >>| function
    | Ok (node, version) -> begin
      printf
	"Server info - (%s, %s)\n"
	(option_to_hex node)
	(option_to_hex version);
      shutdown 0
    end
    | Error _ -> begin
      printf "Failed\n";
      shutdown 1
    end

let () =
  ignore (perform_ping ());
  never_returns (Scheduler.go ())
