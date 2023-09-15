(lang dune 3.0)
(name kxclib)
(version ~dev)

(license MIT)
(maintainers "Haochen M. Kotoi-Xie <hx@kxc.inc>" "KXC Members <info@kxc.inc>")
(authors "Haochen M. Kotoi-Xie" "KXC Members and Contributors")

(source
 (github kxcinc/kxclib-ocaml))
(bug_reports "https://github.com/kxcinc/kxclib-ocaml/issues")

(generate_opam_files true)
(package
 (name kxclib)
 (synopsis "KXC maintained common utilities")
 (description "Common OCaml utilities maintained by Kotoi-Xie Consultancy, Inc.")
 (depends
  (ppx_optcomp :build)
  (ppx_deriving :build)

  (alcotest :with-test)
  (qtest :with-test)
  (js_of_ocaml :with-test)
  (js_of_ocaml-ppx :with-test)
  (prr :with-test)

  (ocaml (>= 4.11.0))))
