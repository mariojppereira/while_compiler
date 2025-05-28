(lang dune 3.17)

(using menhir 2.0)

(name while_compiler)

(version 0.1)

(generate_opam_files true)

(source
 (github mariojppereira/while_compiler))

(authors "Mário Pereira <mjp.pereira@fct.unl.pt>")

(maintainers "Mário Pereira <mjp.pereira@fct.unl.pt>")

(license MIT)

(package
 (name while_compiler)
 (synopsis "A compiler for the While language")
 (description "A compiler from the While language into X86-64 assembly code")
 (depends ocaml)
 (tags
  ("compilers" "while" "x86")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
