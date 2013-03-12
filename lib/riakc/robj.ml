open Core.Std

module Pair = struct
  type t = { key : string
	   ; value : string option
	   }

  let key t   = t.key
  let value t = t.value
end

module Link = struct
  type t = unit
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
	   ; deleted          : bool
	   }

  let of_content pb =
    let module Robj = Pb_robj.Robj in
    let module C    = Pb_robj.Content in
    let pair_of_pair { Pb_robj.Pair.key; value } =
      { Pair.key = key; value = value }
    in
    let usermeta =
      List.map
	~f:pair_of_pair
	pb.C.usermeta
    in
    let indexes =
      List.map
	~f:pair_of_pair
	pb.C.indexes
    in
    { value            = pb.C.value
    ; content_type     = pb.C.content_type
    ; charset          = pb.C.content_type
    ; content_encoding = pb.C.content_encoding
    ; vtag             = pb.C.vtag
    ; links            = [()]
    ; last_mod         = pb.C.last_mod
    ; last_mod_usec    = pb.C.last_mod_usec
    ; usermeta         = usermeta
    ; indexes          = indexes
    ; deleted          = Option.value ~default:false pb.C.deleted
  }

  let of_pb = List.map ~f:(of_content)

  let value t            = t.value
  let content_type t     = t.content_type
  let charset t          = t.charset
  let content_encoding t = t.content_encoding
  let vtag t             = t.vtag
  let last_mod t         = t.last_mod
  let last_mod_usec t    = t.last_mod_usec
  let usermeta t         = t.usermeta
  let indexes t          = t.indexes
  let deleted t          = t.deleted
end

type t = { contents  : Content.t list
	 ; vclock    : string option
	 ; unchanged : bool
	 }

let of_pb pb =
  let module Robj = Pb_robj.Robj in
  let contents = Content.of_pb pb.Robj.contents in
  { contents  = contents
  ; vclock    = pb.Robj.vclock
  ; unchanged = Option.value ~default:false pb.Robj.unchanged
  }

let to_pb t  = failwith "nyi"

let contents t  = t.contents
let vclock t    = t.vclock
let unchanged t = t.unchanged
