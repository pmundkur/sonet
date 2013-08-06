(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2011      Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public License    *)
(*  as published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or (at your option) any later version.                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open Ocamlbuild_plugin

let headers = ["eventloop/eventloop.h"; "eventloop/posix_stubs.h"];;
let libeventloop = "eventloop/libeventloop." ^ !Options.ext_lib;;
let libodbus = "protocols/dbus/libodbus." ^ !Options.ext_lib;;

dispatch begin function
  | After_rules ->
      (* Set up include paths *)
      Pathname.define_context "eventloop" ["eventloop"];
      Pathname.define_context "protocols/http" ["eventloop"; "protocols/http"];
      Pathname.define_context "protocols/http/tests" ["eventloop"; "protocols/http"];
      Pathname.define_context "protocols/dbus" ["eventloop"];
      Pathname.define_context "protocols/dbus/tests" ["eventloop"; "protocols/dbus"];
      Pathname.define_context "protocols/bencode/tests" ["protocols/bencode"];
      Pathname.define_context "codegen/json_convert/tests" ["codegen/json_convert"];

      (* Handle the C stubs in eventloop *)
      flag ["link"; "library"; "ocaml"; "byte"; "use_libeventloop"]
        (S[A"-dllib"; A"-leventloop"; A"-cclib"; A"-leventloop"]);
      flag ["link"; "library"; "ocaml"; "native"; "use_libeventloop"]
        (S[A"-cclib"; A"-leventloop"]);
      dep ["link"; "ocaml"; "use_libeventloop"] [libeventloop];
      (* As an approximation, all C files use the headers.
         Note: This will import headers in the build directory. *)
      dep ["compile"; "c"] headers;

      (* Define the eventloop library and its link behaviour *)
      ocaml_lib ~dir:"eventloop" "eventloop/eventloop";
      flag ["link"; "ocaml"; "byte"; "use_eventloop"]
        (S[A"-custom"; A"-I"; P"eventloop"]);
      flag ["link"; "ocaml"; "native"; "use_eventloop"]
        (S[A"-I"; P"eventloop"]);

      (* Handle the C stubs in dbus *)
      flag ["link"; "library"; "ocaml"; "byte"; "use_libodbus"]
        (S[A"-dllib"; A"-lodbus"; A"-cclib"; A"-lodbus"]);
      flag ["link"; "library"; "ocaml"; "native"; "use_libodbus"]
        (S[A"-cclib"; A"-lodbus"]);
      dep ["link"; "ocaml"; "use_libodbus"] [libodbus];

      (* Define the dbus library and its link behaviour *)
      ocaml_lib ~dir:"protocols/dbus" "protocols/dbus/dbuslib";
      flag ["link"; "ocaml"; "byte"; "use_dbuslib"]
        (S[A"-custom"; A"-I"; P"protocols/dbus"]);
      flag ["link"; "ocaml"; "native"; "use_dbuslib"]
        (S[A"-I"; P"protocols/dbus"]);

      (* TODO: generated files in json_convert/tests and jsonrpc *)

  | _ -> ()
end
