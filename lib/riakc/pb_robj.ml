open Core.Std

module P = Protobuf.Parser

open P.Monad_infix

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
end

module Pair = struct
  type t = { key   : string
	   ; value : string option
	   }

  let parse =
    P.bytes     1 >>= fun key ->
    P.bytes_opt 2 >>= fun value ->
    P.return { key; value }
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
	   ; indexes          : Pair.t list
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
    P.embd_msg_rep 10 Pair.parse >>= fun indexes ->
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
	     ; indexes
	     ; deleted
	     }
end

module Robj = struct
  type t = { contents  : Content.t list
	   ; vclock    : string option
	   ; unchanged : bool option
	   }

  let parse =
    P.embd_msg_rep 1 Content.parse >>= fun contents ->
    P.bytes_opt    2               >>= fun vclock ->
    P.bool_opt     3               >>= fun unchanged ->
    P.return { contents; vclock; unchanged }
end
