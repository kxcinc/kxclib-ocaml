(lang dune 3.9)
(name kxclib)
(version ~dev)

(using directory-targets 0.1)
(using melange 0.1)

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

  (ocaml (>= 4.13.0))))

(package
 (name kxclib-jsoo)
 (synopsis "Js_of_ocaml version of kxclib")
 (description "Js_of_ocaml version of kxclib - Common OCaml utilities maintained by Kotoi-Xie Consultancy, Inc.")
 (depends
  (kxclib (= version))

  prr
  js_of_ocaml
  js_of_ocaml-ppx

  (ocaml (>= 4.13.0))))

(package
 (name kxclib-melange)
 (synopsis "Melange version of kxclib")
 (description "Melange version of kxclib - Common OCaml utilities maintained by Kotoi-Xie Consultancy, Inc.")
 (depends
  (kxclib (= version))
  (melange (>= 2.0.0))))
