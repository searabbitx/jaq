(lang dune 3.16)

(name jaq)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jaq)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune yojson ocolor getopt)
 (tags
  (topics "to describe" your project)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
