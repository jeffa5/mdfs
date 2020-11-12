open Rresult

type tree =
  | File of string * Omd.doc
  | Dir of string * tree list
      (** [t] is the representation of the filesystem tree in terms of parsed documents *)

type t = tree list

val parse : Fpath.t -> (t, R.msg) result

val process : Fpath.t -> t -> t

val render : Fpath.t -> t -> (unit, R.msg) result
