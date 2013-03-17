open Core.Std
open Async.Std

module Rconn = Riakc.Conn
module Robj  = Riakc.Robj

open Deferred.Result.Monad_infix

module State = struct
  type t = unit

  let create () = ()
end

module Rand = struct
  let lowercase = "abcdefghijklmnopqrstuvwxyz"
  let alpha     = lowercase ^ String.uppercase lowercase
  let num       = "0123456789"
  let alphanum  = alpha ^ num
  let symbols   = "!@#$%^&*();:<>,.?"
  let all       = alphanum ^ symbols

  let pick_n src len =
    let s   = String.create len in
    let s_l = String.length src in
    for i = 0 to len - 1 do
      String.set s i (String.get src (Random.int s_l))
    done;
    s

  let key = pick_n alpha
end

let assert_cond msg = function
  | true  -> Deferred.return (Ok ())
  | false -> begin
    printf "Error: %s\n" msg;
    Deferred.return (Error `Assert_failed)
  end

let ping_test c =
  Rconn.ping c >>= fun _ ->
  Deferred.return (Ok ())

let client_id_test c =
  Rconn.client_id c >>= fun _ ->
  Deferred.return (Ok ())

let server_info_test c =
  Rconn.server_info c >>= fun _ ->
  Deferred.return (Ok ())

let list_buckets_test c =
  Rconn.list_buckets c >>= fun _ ->
  Deferred.return (Ok ())

let list_keys_test c =
  let b = Sys.argv.(3) in
  Rconn.list_keys c b >>= fun keys1 ->
  let robj =
    Robj.set_content
      (Robj.Content.create "foobar")
      (Robj.create [])
  in
  Rconn.put c ~b ~k:(Rand.key 10) robj >>= fun _ ->
  Rconn.list_keys c b >>= fun keys2 ->
  assert_cond
    "Key not added"
    (List.length keys1 = (List.length keys2 - 1))
  >>= fun _ ->
  assert_cond
    "Key not in list"
    (List.mem keys2 "foobar")

let get_notfound_test c =
  let b = Sys.argv.(3) in
  let open Deferred.Monad_infix in
  Rconn.get c ~b "no_key_here" >>= function
    | Error `Notfound ->
      Deferred.return (Ok ())
    | Error err ->
      Deferred.return (Error err)
    | Ok _ ->
      Deferred.return (Error `Bad_response)

let get_found_test c =
  let b = Sys.argv.(3) in
  let robj =
    Robj.set_content
      (Robj.Content.create "foobar")
      (Robj.create [])
  in
  let key = Rand.key 10 in
  Rconn.put c ~b ~k:key robj >>= fun (_, _) ->
  Rconn.get c ~b key         >>= fun robj ->
  Deferred.return (Ok ())

let put_return_body_test c =
  let b = Sys.argv.(3) in
  let open Riakc.Opts.Put in
  let robj =
    Robj.set_content
      (Robj.Content.create "foobar")
      (Robj.create [])
  in
  let key = Rand.key 10 in
  Rconn.put c ~opts:[Return_body] ~b ~k:key robj            >>= fun (robj', key) ->
  assert_cond "Key created for unknown reason" (key = None) >>= fun _ ->
  assert_cond
    "Add created sibling"
    (List.length (Robj.contents robj) = List.length (Robj.contents robj'))

let tests = [ ("ping"           , ping_test)
	    ; ("client_id"      , client_id_test)
	    ; ("server_info"    , server_info_test)
	    ; ("list_buckets"   , list_buckets_test)
	    ; ("list_keys"      , list_keys_test)
	    ; ("get_notfound"   , get_notfound_test)
	    ; ("get_found"      , get_found_test)
	    ; ("put_return_body", put_return_body_test)
	    ]

let execute_test t =
  let with_conn () =
    Rconn.with_conn
      ~host:Sys.argv.(1)
      ~port:(Int.of_string Sys.argv.(2))
      t
  in
  with_conn ()

let rec execute_tests s = function
  | [] -> Deferred.return (shutdown 0)
  | (name, t)::ts -> begin
    let open Deferred.Monad_infix in
    execute_test t >>= function
      | Ok () -> begin
	printf "%s...PASSED\n" name;
	execute_tests s ts
      end
      | Error _ -> begin
	printf "%s...FAILED\n" name;
	execute_tests s ts
      end
  end

let run_tests () =
  execute_tests (State.create ()) tests

let () =
  Random.self_init ();
  ignore (run_tests ());
  never_returns (Scheduler.go ())
