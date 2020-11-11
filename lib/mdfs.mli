open Rresult

val convert :
  out:Fpath.t option -> open_:bool -> Fpath.t -> (unit, R.msg) result
