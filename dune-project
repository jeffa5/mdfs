(lang dune 2.6)

(name mdfs)

(implicit_transitive_deps false)

(generate_opam_files true)

(authors "Andrew Jeffery <dev@jeffas.io>")

(maintainers "Andrew Jeffery <dev@jeffas.io>")

(source
 (github jeffa5/mdfs))

(package
 (name mdfs)
 (synopsis "Render a tree of markdown to html")
 (description
   "mdfs takes a filesystem tree of markdown files, processes them to convert links and outputs a corresponding filetree of html files")
 (depends
  (fpath
   (>= 0.7.0))
  (omd
   (>= 2.0.0))
  (logs
   (>= 0.7.0))
  (bos
   (>= 0.2.0))
  (rresult
   (>= 0.6.0))
  (cmdliner
   (>= 1.0.0))
  (fmt
   (>= 0.8.0))
  (lwt
   (>= 5.3.0)))
 (tags
  (cli markdown filesystem html)))
