(lang dune 3.7)

(name ocaml_gpt)

(generate_opam_files true)

(source
 (github PizieDust/ocaml-gpt))

(authors "PizieDust <playersrebirth@gmail.com>")

(maintainers "PizieDust <playersrebirth@gmail.com>")

(license LICENSE)

(documentation https://github.com/PizieDust/ocaml-gpt)

(package
 (name ocaml_gpt)
 (synopsis "A simple library to manipulate GUID Partition tables")
 (description "A longer description")
 (depends ocaml dune cstruct uuidm checkseum)
 (tags
  ("mirage" "ocaml" "storage" "gpt")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
