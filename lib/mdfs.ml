open Lwt.Syntax

let convert ~out dir =
  let* t = Tree.parse dir in
  let t = Tree.process t in
  Tree.render out t
