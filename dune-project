(lang dune 2.9)

(name Sudoku_OCaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name Sudoku_OCaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml-version (>= 4.14.0))
  (dune (>= 2.9.0))
  (lablgtk3 (>= 3.1.2))
  (cairo2 (>= 0.6.2))
  (cairo2-gtk (>= 0.6.2))
  (alcotest :with-test)
  (qcheck :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
