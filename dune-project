(lang dune 3.17)

(name fp_lab3)

(generate_opam_files true)

(source
 (github pmpknu/fp_lab3))

(authors "Dan Gorlyakov")

(maintainers "Dan Gorlyakov")

(license MIT)

(package
 (name fp_lab3)
 (synopsis "Interpolation in OCaml")
 (description "A third laboratory work for Functional Programming course at ITMO University")
 (depends ocaml ocamlformat core ppx_inline_test ppx_expect ppx_deriving alcotest qcheck qcheck-alcotest)
)
