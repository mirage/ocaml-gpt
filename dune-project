(lang dune 3.7)

(name gpt)

(generate_opam_files true)

(source
 (github mirage/ocaml-gpt))

(authors "PizieDust <playersrebirth@gmail.com>")

(maintainers "PizieDust <playersrebirth@gmail.com>" "Reynir Björnsson <reynir@reynir.dk>")

(license ISC)

(documentation https://mirage.github.io/ocaml-gpt/)

(package
 (name gpt)
 (synopsis "A library to manipulate GUID Partition tables")
 (description "A longer description")
 (depends (ocaml (>= "4.13.0")) dune cstruct (uuidm (>= "0.9.7")) (checkseum (>= "0.4.0")) (mbr-format (>= "2.0.0")) (alcotest :with-test) (fmt :with-test))
 (tags
  ("mirage" "ocaml" "storage" "gpt")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
