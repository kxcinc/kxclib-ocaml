## Get started

to use Kxclib in a new project that uses dune, simply do the following:

```
git submodule add https://github.com/kxcteam/kxclib-ocaml.git vendors/kxclib
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
