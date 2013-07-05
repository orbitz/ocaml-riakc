open Core.Std

let option_of_bool = function
  | true  -> Some true
  | false -> None

let int_of_string i = Option.try_with (fun () -> Int.of_string i)

module Usermeta = struct
  type t = { key : string
	   ; value : string option
	   }

  let create ~k ~v = { key = k; value = v }

  let key t   = t.key
  let value t = t.value

  let set_key s t = {t with key = s}
  let set_value so t = {t with value = so}

  let of_pb { Pb_robj.Pair.key; value } =
    { key; value }

  let to_pb { key; value } =
    { Pb_robj.Pair.key; value }

end

module Index = struct
  type idx = | String  of string
	     | Integer of int
	     | Bad_int of string
	     | Unknown of string

  type t = { key : string
	   ; value : idx
	   }

  let create ~k ~v = { key = k; value = v }

  let key t   = t.key
  let value t = t.value

  let set_key s t = {t with key = s}
  let set_value idx t = {t with value = idx}

  let of_pb { Pb_robj.Pair.key; value } =
    let value = Option.value ~default:"" value in
    match String.rsplit2 ~on:'_' key with
      | Some (k, "bin") ->
	{ key = k; value = String value }
      | Some (k, "int") -> begin
	match int_of_string value with
	  | Some i ->
	    { key = k; value = Integer i }
	  | None ->
	    { key = k; value = Bad_int value }
      end
      | Some (_, _) ->
	{ key; value = Unknown value }
      | None ->
	{ key; value = Unknown value }

  let to_pb = function
    | { key; value = String s } ->
      { Pb_robj.Pair.key = key ^ "_bin"; value = Some s }
    | { key; value = Integer i } ->
      { Pb_robj.Pair.key = key ^ "_int"; value = Some (Int.to_string i) }
    | { key; value = Bad_int s } ->
      { Pb_robj.Pair.key; value = Some s }
    | { key; value = Unknown s } ->
      { Pb_robj.Pair.key; value = Some s }

end

module Link = struct
  type t = { bucket : string option
	   ; key    : string option
	   ; tag    : string option
	   }

  let bucket t = t.bucket
  let key t    = t.key
  let tag t    = t.tag

  let set_bucket b t = { t with bucket = b }
  let set_key k t    = { t with key = k }
  let set_tag tag t  = { t with tag = tag }

  let of_pb l =
    let module L = Pb_robj.Link in
    { bucket = l.L.bucket
    ; key    = l.L.key
    ; tag    = l.L.tag
    }

  let to_pb t =
    { Pb_robj.Link.bucket = t.bucket
    ;              key    = t.key
    ;              tag    = t.tag
    }

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
	   ; usermeta         : Usermeta.t list
	   ; indices          : Index.t list
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
    ; usermeta         = List.map ~f:Usermeta.of_pb pb.C.usermeta
    ; indices          = List.map ~f:Index.of_pb pb.C.indices
    ; deleted          = Option.value ~default:false pb.C.deleted
    }

  let to_pb c =
    let module C = Pb_robj.Content in
    { C.value = c.value
    ;   content_type     = c.content_type
    ;   charset          = c.charset
    ;   content_encoding = c.content_encoding
    ;   vtag             = c.vtag
    ;   links            = List.map ~f:Link.to_pb c.links
    ;   last_mod         = c.last_mod
    ;   last_mod_usec    = c.last_mod_usec
    ;   usermeta         = List.map ~f:Usermeta.to_pb c.usermeta
    ;   indices          = List.map ~f:Index.to_pb c.indices
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
    ; indices          = []
    ; deleted          = false
    }

  let value t            = t.value
  let content_type t     = t.content_type
  let charset t          = t.charset
  let content_encoding t = t.content_encoding
  let vtag t             = t.vtag
  let links t            = t.links
  let last_mod t         = t.last_mod
  let last_mod_usec t    = t.last_mod_usec
  let usermeta t         = t.usermeta
  let indices t          = t.indices
  let deleted t          = t.deleted

  let set_value v t             = { t with value = v }
  let set_content_type ct t     = { t with content_type = ct }
  let set_charset cs t          = { t with charset = cs }
  let set_content_encoding ce t = { t with content_encoding = ce }
  let set_vtag vt t             = { t with vtag = vt }
  let set_links ls t            = { t with links = ls }
  let set_last_mod lm t         = { t with last_mod = lm }
  let set_last_mod_usec lmu t   = { t with last_mod_usec = lmu }
  let set_usermeta u t          = { t with usermeta = u }
  let set_indices i t           = { t with indices = i }


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

let create c =
  { contents  = [c]
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
