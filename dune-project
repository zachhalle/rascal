(lang dune 3.3)

(name rascal)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Zach Halle")

(maintainers "Zach Halle")

(license LICENSE)

(using menhir 2.1)

(package
 (name rascal)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
