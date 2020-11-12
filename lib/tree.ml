type tree = File of string * Omd.doc | Dir of string * tree list

type t = tree list

module Log = (val Logs.src_log (Logs.Src.create "mdfs"))

open Rresult
open Bos

let is_md_file f =
  match Fpath.get_ext f with ".md" | ".markdown" -> true | _ -> false

let parse src_dir =
  Log.info (fun f -> f "Parsing from %a" Fpath.pp src_dir);
  let rec handle_dir dir =
    Log.debug (fun f -> f "Parsing %a" Fpath.pp dir);
    match OS.Dir.contents dir with
    | Error m -> Error m
    | Ok l ->
        let x =
          List.filter_map
            (fun f ->
              match Fpath.to_string f with
              | "." | ".." -> None
              | _ -> (
                  let stat = OS.Path.stat f in
                  match stat with
                  | Error _ as e -> Some e
                  | Ok stat -> (
                      match stat.st_kind with
                      | S_REG ->
                          if is_md_file f then (
                            match OS.File.read f with
                            | Error _ as e -> Some e
                            | Ok s ->
                                Log.debug (fun m -> m "Parsing %a" Fpath.pp f);
                                let doc = Omd.of_string s in
                                let path = Fpath.basename f in
                                Some (Ok (File (path, doc))) )
                          else (
                            Log.debug (fun m -> m "Skipping %a" Fpath.pp f);
                            None )
                      | S_DIR -> (
                          let l = handle_dir f in
                          match l with
                          | Error _ as e -> Some e
                          | Ok l ->
                              let path = Fpath.basename f in
                              Option.some @@ R.ok @@ Dir (path, l) )
                      | _ -> assert false ) ))
            l
        in
        List.fold_right
          (fun t r ->
            match r with
            | Error _ as e -> e
            | Ok l -> ( match t with Error _ as e -> e | Ok t -> Ok (t :: l) ))
          x (Ok [])
  in
  handle_dir src_dir

let md_to_html_links path d =
  let convert_link (l : Omd.link) =
    if Uri.of_string l.destination |> Uri.scheme |> Option.is_some then l
    else
      let f = Fpath.of_string l.destination in
      match f with
      | Error _ -> l
      | Ok f ->
          Logs.debug (fun m -> m "Converting link %a" Fpath.pp f);
          let relative = Fpath.append (Fpath.parent path) f in
          if OS.File.must_exist relative |> R.is_ok |> not then
            Logs.warn (fun m ->
                m "File target does not exist: %a from %a" Fpath.pp relative
                  Fpath.pp path);
          if Fpath.is_rel f && is_md_file f then
            {
              l with
              destination = Filename.remove_extension l.destination ^ ".html";
            }
          else l
  in
  let rec convert_inline (i : Omd.inline) =
    let il_desc =
      match i.il_desc with
      | Link l -> Omd.Link (convert_link l)
      | Image l -> Image (convert_link l)
      | Hard_break -> Hard_break
      | Soft_break -> Soft_break
      | Concat il -> Concat (List.map convert_inline il)
      | Text s -> Text s
      | Emph i -> Emph (convert_inline i)
      | Strong i -> Strong (convert_inline i)
      | Code s -> Code s
      | Html s -> Html s
    in
    { i with il_desc }
  in
  let rec convert_block (b : Omd.block) =
    let bl_desc =
      match b.bl_desc with
      | Paragraph i -> Omd.Paragraph (convert_inline i)
      | Thematic_break -> Thematic_break
      | List (lt, ls, bss) ->
          List (lt, ls, List.map (List.map convert_block) bss)
      | Blockquote bs -> Blockquote (List.map convert_block bs)
      | Heading (d, i) -> Heading (d, convert_inline i)
      | Code_block (s, s') -> Code_block (s, s')
      | Html_block s -> Html_block s
      | Definition_list ds ->
          Definition_list
            (List.map
               (fun (d : Omd.def_elt) ->
                 {
                   Omd.term = convert_inline d.term;
                   defs = List.map convert_inline d.defs;
                 })
               ds)
    in
    { b with bl_desc }
  in
  List.map convert_block d

let process root t =
  Log.info (fun f -> f "Processing");
  let process_doc path d = md_to_html_links path d in
  let rec process_tree dir = function
    | File (f, o) ->
        Log.debug (fun m -> m "Processing file %s" f);
        File (f, process_doc (Fpath.add_seg dir f) o)
    | Dir (f, l) ->
        Log.debug (fun m -> m "Processing dir %s" f);
        Dir (f, List.map (process_tree (Fpath.add_seg dir f)) l)
  in
  List.map (process_tree root) t

let mkdirf dir = OS.Dir.create ~path:true dir |> R.map (fun _ -> ())

let render dir t =
  Log.info (fun f -> f "Rendering to %a" Fpath.pp dir);
  let rec render_subtree dir t =
    match t with
    | File (f, o) ->
        let fn = Fpath.add_seg dir f |> Fpath.set_ext "html" in
        Log.debug (fun m -> m "Rendering file %s to %a" f Fpath.pp fn);
        let h = Omd.to_html o in
        OS.File.write fn h
    | Dir (f, l) ->
        let fn = Fpath.add_seg dir f in
        Log.debug (fun m -> m "Rendering dir %s to %a" f Fpath.pp fn);
        mkdirf fn >>= fun () ->
        List.fold_left
          (fun r t ->
            match r with Error e -> Error e | Ok () -> render_subtree fn t)
          (Ok ()) l
  in
  ignore @@ mkdirf dir;
  List.fold_left
    (fun r t ->
      match r with Error e -> Error e | Ok () -> render_subtree dir t)
    (Ok ()) t
