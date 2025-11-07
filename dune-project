(lang dune 3.20)

(name clispy)

(generate_opam_files true)

(source
 (github dragsbruh/clipsy))

(authors "dragsbruh <dragsbruh@github.com>")

(maintainers "dragsbruh <dragsbruh@github.com>")

(license MIT)

(documentation https://github.com/dragsbruh/clispy)

(package
 (name clispy)
 (synopsis "A lisp-like programming language for a beam-like vm (todo)")
 (description
  "A very silly make-(a-)lisp that (in the future) runs on a custom beam-like vm")
 (depends ocaml)
 (tags
  (mal beam erlang lisp language zig)))
