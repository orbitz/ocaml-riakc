open Core.Std

let option_of_bool = function
  | true  -> Some true
  | false -> None

module Pair = struct
  type t = { key : string
	   ; value : string option
	   }

  let key t   = t.key
  let value t = t.value

  let of_pb { Pb_robj.Pair.key; value } =
    { key; value }

  let to_pb { key; value } =
    { Pb_robj.Pair.key; value }

end

module Link = struct
  type t = unit

  let of_pb _ = ()

  let to_pb _ = ()
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

  let of_pb pb =
    let module C = Pb_robj.Content in
    { value            = pb.C.value
    ; content_type     = pb.C.content_type
    ; charset          = pb.C.content_type
    ; content_encoding = pb.C.content_encoding
    ; vtag             = pb.C.vtag
    ; links            = List.map ~f:Link.of_pb pb.C.links
    ; last_mod         = pb.C.last_mod
    ; last_mod_usec    = pb.C.last_mod_usec
    ; usermeta         = List.map ~f:Pair.of_pb pb.C.usermeta
    ; indexes          = List.map ~f:Pair.of_pb pb.C.indexes
    ; deleted          = Option.value ~default:false pb.C.deleted
    }

  let to_pb c =
    let module C = Pb_robj.Content in
    { C.value = c.value
    ;   content_type     = c.content_type
    ;   charset          = c.charset
    ;   content_encoding = c.content_encoding
    ;   vtag             = c.vtag
    ;   links            = []
    ;   last_mod         = c.last_mod
    ;   last_mod_usec    = c.last_mod_usec
    ;   usermeta         = List.map ~f:Pair.to_pb c.usermeta
    ;   indexes          = List.map ~f:Pair.to_pb c.indexes
    ;   deleted          = option_of_bool c.deleted
    }

  let create v =
    { value = v
    ; content_type     = None
    ; charset          = None
    ; content_encoding = None
    ; vtag             = None
    ; links            = []
    ; last_mod         = None
    ; last_mod_usec    = None
    ; usermeta         = []
    ; indexes          = []
    ; deleted          = false
    }

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

  let set_value v t             = { t with value = v }
  let set_content_type ct t     = { t with content_type = ct }
  let set_charset cs t          = { t with charset = cs }
  let set_content_encoding ce t = { t with content_encoding = ce }
  let set_vtag vt t             = { t with vtag = vt }
  let set_last_mod lm t         = { t with last_mod = lm }
  let set_last_mod_usec lmu t   = { t with last_mod_usec = lmu }
  let set_usermeta u t          = { t with usermeta = u }
  let set_indexes i t           = { t with indexes = i }


end

type 'a t = { contents  : Content.t list
	    ; vclock    : string option
	    ; unchanged : bool
	    }

let of_pb contents vclock unchanged =
  let contents = List.map ~f:Content.of_pb contents in
  { contents  = contents
  ; vclock    = vclock
  ; unchanged = Option.value ~default:false unchanged
  }

let to_pb t =
  (List.map ~f:Content.to_pb t.contents, t.vclock)

let create cs =
  { contents  = cs
  ; vclock    = None
  ; unchanged = false
  }

let contents t        = t.contents
let content t         = List.hd_exn (t.contents)
let vclock t          = t.vclock
let unchanged t       = t.unchanged

let set_contents cs t = { t with contents = cs }
let set_content c t   = { t with contents = [c] }
let set_vclock v t    = { t with vclock = v }
