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
 (depends)
 (tags
  (cli markdown filesystem html)))
