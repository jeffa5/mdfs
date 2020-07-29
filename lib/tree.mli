type subtree =
  | File of string * Omd.doc
  | Dir of string * subtree list
      (** [t] is the representation of the filesystem tree in terms of parsed documents *)

type t = subtree list

val parse : string -> t Lwt.t

val process : t -> t

val render : string -> t -> unit Lwt.t
