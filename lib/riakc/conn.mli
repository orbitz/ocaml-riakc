open Core.Std
open Async.Std

type t

type error = [ `Bad_conn ]

module Quorum : sig
  type t =
    | One
    | All
    | Default
    | Quorum
    | N of int

end

module Get_opts : sig
  type error =[ `Bad_conn | Response.error ]

  type t =
    | Timeout of int
    | Quorum_read of Quorum.t
end

module Put_opts : sig
  type error = [ `Bad_conn
	       | `Not_found
	       ]

  type t =
    | Timeout of int
    | Quorum_write of Quorum.t
end

val connect : host:string -> port:int -> (t, [> error ]) Deferred.Result.t
val close   : t -> (unit, [> error ]) Deferred.Result.t
val ping    : t -> (unit, [> error | Response.error ]) Deferred.Result.t

val get :
  t ->
  ?opts:Get_opts.t list ->
  b:string ->
  k:string ->
  (Robj.t, [> Get_opts.error ]) Deferred.Result.t

val put :
  t ->
  ?opts:Put_opts.t list ->
  obj:Robj.t ->
  (Robj.t, [> Put_opts.error ]) Deferred.Result.t
