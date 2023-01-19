(lang dune 3.0)
(name kxclib)
(version ~dev)

(license MIT)
(maintainers "Haochen Xie <hx@kxc.io>" "KXC Members <info@kxc.io>")
(authors "Haochen Xie" "KXC Members and Contributors")

(source
 (github kxcteam/kxclib-ocaml))
(bug_reports "https://github.com/kxcteam/kxclib-ocaml/issues")

(generate_opam_files true)
(package
 (name kxclib)
 (synopsis "KXC maintained common utilities")
 (description "Common OCaml utilities maintained by Kotoi-Xie Consultancy")
 (depends
  (ppx_optcomp :build)
  (ppx_deriving :build)

  (alcotest :with-test)
  (qtest :with-test)

  (ocaml (>= 4.11.0))))
