open Core.Std

module P = Protobuf.Parser
module B = Protobuf.Builder

open P.Monad_infix

let option_of_bool = function
  | Some true -> Some true
  | _         -> None

module Link = struct
  type t = { bucket : string option
	   ; key    : string option
	   ; tag    : string option
	   }

  let parse =
    P.bytes_opt 1 >>= fun bucket ->
    P.bytes_opt 2 >>= fun key ->
    P.bytes_opt 3 >>= fun tag ->
    P.return { bucket; key; tag }

  let build t =
    let open Result.Monad_infix in
    let b = B.create () in
    B.bytes_opt b 1 t.bucket >>= fun () ->
    B.bytes_opt b 2 t.key    >>= fun () ->
    B.bytes_opt b 3 t.tag    >>= fun () ->
    Ok (B.to_string b)
end

module Pair = struct
  type t = { key   : string
	   ; value : string option
	   }

  let parse =
    P.bytes     1 >>= fun key ->
    P.bytes_opt 2 >>= fun value ->
    P.return { key; value }

  let build t =
    let open Result.Monad_infix in
    let b = B.create () in
    B.bytes     b 1 t.key   >>= fun () ->
    B.bytes_opt b 2 t.value >>= fun () ->
    Ok (B.to_string b)
end

module Content = struct
  type t = { value            : string
	   ; content_type     : string option
	   ; charset          : string option
	   ; content_encoding : string option
	   ; vtag             : string option
	   ; links            : Link.t list
	   ; last_mod         : Int32.t option
	   ; last_mod_usec    : Int32.t option
	   ; usermeta         : Pair.t list
	   ; indices          : Pair.t list
	   ; deleted          : bool option
	   }

  let parse =
    P.bytes         1            >>= fun value ->
    P.bytes_opt     2            >>= fun content_type ->
    P.bytes_opt     3            >>= fun charset ->
    P.bytes_opt     4            >>= fun content_encoding ->
    P.bytes_opt     5            >>= fun vtag ->
    P.embd_msg_rep  6 Link.parse >>= fun links ->
    P.int32_opt     7            >>= fun last_mod ->
    P.int32_opt     8            >>= fun last_mod_usec ->
    P.embd_msg_rep  9 Pair.parse >>= fun usermeta ->
    P.embd_msg_rep 10 Pair.parse >>= fun indices ->
    P.bool_opt     11            >>= fun deleted ->
    P.return { value
	     ; content_type
	     ; charset
	     ; content_encoding
	     ; vtag
	     ; links
	     ; last_mod
	     ; last_mod_usec
	     ; usermeta
	     ; indices
	     ; deleted
	     }

  let build t =
    let open Result.Monad_infix in
    let b = B.create () in
    B.bytes        b  1 t.value                    >>= fun () ->
    B.bytes_opt    b  2 t.content_type             >>= fun () ->
    B.bytes_opt    b  3 t.charset                  >>= fun () ->
    B.bytes_opt    b  4 t.content_encoding         >>= fun () ->
    B.bytes_opt    b  5 t.vtag                     >>= fun () ->
    B.embd_msg_rep b  6 t.links Link.build         >>= fun () ->
    B.int32_opt    b  7 t.last_mod                 >>= fun () ->
    B.int32_opt    b  8 t.last_mod_usec            >>= fun () ->
    B.embd_msg_rep b  9 t.usermeta Pair.build      >>= fun () ->
    B.embd_msg_rep b 10 t.indices Pair.build       >>= fun () ->
    B.bool_opt     b 11 (option_of_bool t.deleted) >>= fun () ->
    Ok (B.to_string b)
end
