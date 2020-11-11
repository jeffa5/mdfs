module Log = (val Logs.src_log (Logs.Src.create "mdfs"))

open Rresult

let convert ~out ~open_ dir =
  let cur = Unix.getcwd () |> Fpath.v in
  let dir = if Fpath.is_abs dir then dir else Fpath.append cur dir in
  let out =
    match out with
    | Some out -> if Fpath.is_abs out then out else Fpath.append cur out
    | None -> Fpath.add_seg dir "_html"
  in
  Log.info (fun f -> f "Converting %a into %a" Fpath.pp dir Fpath.pp out);
  Bos.OS.Dir.delete ~recurse:true out >>= fun () ->
  Tree.parse dir >>| Tree.process >>= Tree.render out >>= fun () ->
  if open_ then Bos.Cmd.(v "xdg-open" % Fpath.to_string out) |> Bos.OS.Cmd.run
  else Ok ()
