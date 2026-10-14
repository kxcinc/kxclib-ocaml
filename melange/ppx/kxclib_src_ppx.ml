(* standalone ppxlib driver producing melange/kxclib_src.pp.ml.

   the rewriters linked here must match what melange/dune used to pass as
   (preprocess (pps ...)) on the kxclib_src_melange library. that library only
   ever existed so dune would emit the .pp.ml rule as a side effect; it could
   never be compiled, since the derived code resolves Ppx_deriving_runtime only
   once melange/src/kxclib_comp_mel.ml is prepended. *)
let () = Ppxlib.Driver.standalone ()
