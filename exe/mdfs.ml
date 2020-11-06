open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let fpath (c_parse, _c_print) =
  let parse s =
    match c_parse s with
    | `Error _ as e -> e
    | `Ok s -> (
        match Fpath.of_string s with
        | Error (`Msg e) -> `Error e
        | Ok f -> `Ok f )
  in
  let print = Fpath.pp in
  (parse, print)

let dir =
  let doc = "Directory to build the html from" in
  let docv = "DIR" in
  Arg.(required & pos 0 (some (fpath dir)) None & info ~doc ~docv [])

let out =
  let doc = "Directory to output the html to" in
  let docv = "DIR" in
  Arg.(value & opt (some (fpath string)) None & info ~doc ~docv [ "o" ])

let main dir out () =
  match Mdfs_lib.Mdfs.convert dir ~out with
  | Ok () -> Ok ()
  | Error (`Msg m) -> Error (`Msg m)

let main_t = Term.(term_result (const main $ dir $ out $ setup_log))

let () = Term.exit @@ Term.eval (main_t, Term.info "mdfs")
