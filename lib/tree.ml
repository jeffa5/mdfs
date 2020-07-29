type subtree = File of string * Omd.doc | Dir of string * subtree list

type t = subtree list

open Lwt.Syntax

module Log = (val Logs.src_log (Logs.Src.create "mdfs"))

let is_md_file f =
  match Filename.extension f with ".md" | ".markdown" -> true | _ -> false

let parse dir =
  let rec handle_dir dir =
    Lwt_unix.files_of_directory dir
    |> Lwt_stream.filter_map_s (fun f ->
           match f with
           | "." | ".." -> Lwt.return_none
           | _ -> (
               let fn = Filename.concat dir f in
               let* stat = Lwt_unix.stat fn in
               match stat.st_kind with
               | S_REG -> (
                   match Filename.extension f with
                   | ".md" | ".markdown" ->
                       let+ s =
                         let+ lines =
                           Lwt_io.lines_of_file fn |> Lwt_stream.to_list
                         in
                         lines |> String.concat "\n"
                       in
                       Log.info (fun f -> f "Parsing %s" fn);
                       let doc = Omd.of_string s in
                       Option.some @@ File (f, doc)
                   | _ ->
                       Log.info (fun f ->
                           f "Skipping %s" (Filename.concat dir fn));
                       Lwt.return_none )
               | S_DIR ->
                   Log.info (fun f -> f "Entering %s" fn);
                   let+ l = handle_dir fn in
                   Option.some @@ Dir (f, l)
               | _ -> assert false ))
    |> Lwt_stream.to_list
  in
  handle_dir dir

let md_to_html_links d =
  let convert_link (l : Omd.link) =
    if Filename.is_relative l.destination && is_md_file l.destination then
      { l with destination = Filename.remove_extension l.destination ^ ".html" }
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

let process t =
  Log.info (fun f -> f "Processing");
  let process_doc d = md_to_html_links d in
  let rec process_subtree = function
    | File (f, o) -> File (f, process_doc o)
    | Dir (f, l) -> Dir (f, List.map process_subtree l)
  in
  List.map process_subtree t

let mkdirf dir =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir dir 0o755)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
      | exn -> raise exn)

let render dir t =
  let rec render_subtree dir = function
    | File (f, o) ->
        let fn = Filename.concat dir (Filename.remove_extension f ^ ".html") in
        Log.info (fun f -> f "Rendering %s" fn);
        let h = Omd.to_html o in
        Lwt_stream.of_list (String.split_on_char '\n' h)
        |> Lwt_io.lines_to_file fn
    | Dir (f, l) ->
        let fn = Filename.concat dir f in
        let* () = mkdirf fn in
        Lwt_list.iter_s (fun t -> render_subtree fn t) l
  in
  let* () = mkdirf dir in
  Lwt_list.iter_s (render_subtree dir) t
