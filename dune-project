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
  (js_of_ocaml :with-test)
  (js_of_ocaml-ppx :with-test)
  (prr :with-test)
 
  (ocaml (>= 4.13.0))
  (melange (>= 2.0.0))))
