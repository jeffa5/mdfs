let setup_log style_renderer level =
  let pp_header ppf (level, _so) =
    let style =
      match level with
      | Logs.Error -> `Fg `Red
      | Warning -> `Fg `Yellow
      | Info -> `Fg `Blue
      | Debug -> `Fg `Green
      | App -> `Fg `Cyan
    in
    Fmt.pf ppf "[%a %a] "
      (Ptime.pp_rfc3339 ~frac_s:2 ())
      (Ptime_clock.now ())
      (Fmt.styled style Logs.pp_level)
      level
  in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  ()

open Cmdliner

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

let open_arg =
  let doc = "Open the generated html in a browser" in
  Arg.(value & flag & info ~doc [ "open" ])

let main dir out open_ () =
  match Mdfs_lib.Mdfs.convert dir ~out ~open_ with
  | Ok () -> Ok ()
  | Error (`Msg m) -> Error (`Msg m)

let main_t = Term.(term_result (const main $ dir $ out $ open_arg $ setup_log))

let () = Term.exit @@ Term.eval (main_t, Term.info "mdfs")
