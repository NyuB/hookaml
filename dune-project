(lang dune 3.16)

(name hookaml)

(generate_opam_files true)

(source
 (github NyuB/hookaml))

(authors "NyuB")

(maintainers "NyuB")

(license LICENSE)

(documentation https://github.com/NyuB/hookaml)

(package
 (name hookaml)
 (synopsis "VCS scripting")
 (description "VCS scripting")
 (depends ocaml dune (ocamlformat :with-test) sexplib)
 (tags ()))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
