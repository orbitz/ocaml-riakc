open Core.Std
open Async.Std

type t

type error = [ `Bad_conn ]

val connect   : host:string -> port:int -> (t, [> error ]) Deferred.Result.t
val close     : t -> (unit, [> error ]) Deferred.Result.t

val with_conn :
  host:string ->
  port:int ->
  (unit -> ('a, [> error ]) Deferred.Result.t) ->
  ('a, [> error ]) Deferred.Result.t

val ping        : t -> (unit, [> error | Response.error ]) Deferred.Result.t
val client_id   : t -> (string, [> error | Response.error ]) Deferred.Result.t
val server_info :
  t ->
  ((string option * string option), [> error | Response.error ]) Deferred.Result.t

val list_buckets : t -> (string list, [> error | Response.error ]) Deferred.Result.t
val list_keys    : t -> string -> (string list, [> error | Response.error ]) Deferred.Result.t
val bucket_props : t -> string -> (Response.props, [> error | Response.error ]) Deferred.Result.t

val get :
  t ->
  ?opts:Opts.Get.t list ->
  b:string ->
  k:string ->
  (Robj.t, [> Opts.Get.error ]) Deferred.Result.t

val put :
  t ->
  ?opts:Opts.Put.t list ->
  obj:Robj.t ->
  (Robj.t, [> Opts.Put.error ]) Deferred.Result.t
