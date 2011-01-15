open Ocamlbuild_plugin

(* Adapted from
   http://brion.inria.fr/gallium/index.php/Ocamlbuild_example_with_C_stubs
   and
   http://stackoverflow.com/questions/2374136/ocamlbuild-building-toplevel
*)

(* List of headers *)
let headers = ["eventloop.h"];;

dispatch begin function
  | After_rules ->
      (* eventloop is an ocaml library.
         This will declare use_eventloop and include_eventloop *)
      ocaml_lib "eventloop";

      (* Every ocaml link in bytecode to eventloop will add -custom *)
      flag ["link"; "ocaml"; "byte"; "use_eventloop"] (A"-custom");

      let libeventloop_stubs = "libeventloop_stubs." ^ !Options.ext_lib in

        flag ["link"; "library"; "ocaml"; "byte"; "use_libeventloop"]
          (S[A"-dllib"; A"-leventloop_stubs"; A"-cclib"; A "-leventloop_stubs"]);

        flag ["link"; "library"; "ocaml"; "native"; "use_libeventloop"]
          (S[A"-cclib"; A"-leventloop_stubs"]);

        (* When ocaml links something that uses libeventloop_stubs,
           then one needs that file to be up to date. *)
        dep  ["link"; "ocaml"; "use_libeventloop"] [libeventloop_stubs];

        (* As an approximation all our C files use the headers.
           Note: This will import headers in the build directory. *)
        dep  ["compile"; "c"] headers;
  | _ -> ()
end
