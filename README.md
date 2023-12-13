**todo: better documentation, better stability, better testing, better packaging, better everything..**

## Documentation

Currently, reading [kxclib.mli](intf/kxclib.ml) is probably the best option.

## Get started

to use Kxclib in a new project that uses dune, simply do the following:

```
git submodule add https://github.com/kxcinc/kxclib-ocaml.git vendors/kxclib
echo (vendored_dirs vendors) >> dune
```
at the project root, then add something like the below to the stanza's of your dune files:

```
(library
 (name foo)
 (libraries kxclib)
 (flags (:standard
          -open Kxclib)))
```

## Caveats

- This project is currently mostly meant to be used only in internal projects.
- Changes introducing breakages may be pushed at any time.
- You are advised to pin to a specific commit if you want to use it in your own project for the time being.
