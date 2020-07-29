open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let dir =
  let doc = "Directory to build the html from" in
  let docv = "DIR" in
  Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

let out =
  let doc = "Directory to output the html to" in
  let docv = "DIR" in
  Arg.(value & opt string "_html" & info ~doc ~docv [ "o" ])

let main dir out () = Lwt_main.run @@ Mdfs_lib.Mdfs.convert dir ~out

let main_t = Term.(const main $ dir $ out $ setup_log)

let () = Term.exit @@ Term.eval (main_t, Term.info "mdfs")
