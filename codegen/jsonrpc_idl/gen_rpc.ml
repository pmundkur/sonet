(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License (version
 * 2.1 only) as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Syntax_json_conv

let make_default_output_filename f suffix =
  let dir, base = Filename.dirname f, Filename.basename f in
  let stem = Filename.chop_extension base in
    Filename.concat dir (stem ^ suffix)

exception Invalid_endpoint_option of string
let parse_args () =
  let input = ref "" in
  let endpoints = ref [] in
  let add_ep_output s =
    try
      let colon = String.index s ':' in
      let len = String.length s in
      let ep = String.sub s 0 colon in
      let epfile = String.sub s (colon+1) (len - colon - 1) in
        endpoints := (ep, epfile) :: !endpoints
    with _ -> raise (Invalid_endpoint_option s) in
  let options = [ ("-i", Arg.Set_string input, " input file");
                  ("-o", Arg.String add_ep_output, " endpoint:output_file");
                ] in
  let usage = Printf.sprintf "Usage: %s [options]" Sys.argv.(0) in
  let errmsg s =
    Printf.eprintf "%s\n" s;
    Arg.usage (Arg.align options) usage;
    exit 1
  in
    Arg.parse (Arg.align options) (fun s -> input := s) usage;
    if !input = "" then errmsg "Unspecified input file!";
    !input, !endpoints

let read_whole_file ic =
  let buf = Buffer.create 2048 in
  let str = String.create 1024 in
  let rec do_read () =
    (* Don't use input_line, since it does not preserve newlines. *)
    let read = input ic str 0 (String.length str) in
      match read with
        | 0 -> raise End_of_file
        | _ -> Buffer.add_substring buf str 0 read; do_read ()
  in
    try do_read () with End_of_file -> Buffer.contents buf

let parse_file f =
  let rpc_decls = ref [] in
  let count = ref 1 in
  let ic = open_in f in
  let input = ref (read_whole_file ic) in
  let state = ref (Json_parse.init_parse_state ()) in
    while String.length !input > 0 do
      match Json_parse.parse !state !input with
        | Json_parse.Json_value (v, consumed) ->
            rpc_decls := (!count, v) :: !rpc_decls;
            incr count;
            input := String.sub !input consumed ((String.length !input) - consumed);
            state := Json_parse.init_parse_state ()
        | Json_parse.Json_parse_incomplete st ->
            input := "";
            state := st
    done;
    (match Json_parse.finish_parse !state with
       | Some v -> rpc_decls := (!count, v) :: !rpc_decls;
       | None -> ());
    List.rev !rpc_decls


exception Unknown_rpc_decl of int * Json.t
exception Invalid_rpc_decl of int * (* type *) string * (* msg *) string
exception Undefined_endpoint of string

let print_exception e =
  let msg =
    match e with
      | Json_parse.Parse_error e ->
          Json_parse.string_of_error e
      | Json_conv.Json_conv_error e ->
          Json_conv.string_of_error e
      | Unknown_rpc_decl (i, j) ->
          Printf.sprintf "Rpc declaration #%d is of unknown type." i
      | Invalid_rpc_decl (i, n, m) ->
          Printf.sprintf "Error parsing decl %d for %s: %s" i n m
      | Undefined_endpoint e ->
          Printf.sprintf "Endpoint \"%s\" not found in input file." e
      | Sys_error s ->
          Printf.sprintf "%s" s
      | e ->
          Printf.sprintf "%s" (Printexc.to_string e)
  in
    Printf.eprintf "%s\n" msg

let process_jdecl (i, j) =
  if not (Json.is_object j) then
    raise (Unknown_rpc_decl (i, j));
  let obj = Json_conv.get_object_table j in
    if (Json_conv.is_object_field_present obj "use_modules") then
      try Rpc_decl.Rpc_use (use_of_json j)
      with Json_conv.Json_conv_error err ->
	raise (Invalid_rpc_decl (i, "use", (Json_conv.string_of_error err)))
    else if (Json_conv.is_object_field_present obj "server_name") then
      try Rpc_decl.Rpc_server (server_of_json j)
      with Json_conv.Json_conv_error err ->
	raise (Invalid_rpc_decl (i, "server", (Json_conv.string_of_error err)))
    else if (Json_conv.is_object_field_present obj "rpc_type") then
      try Rpc_decl.Rpc_rpc (rpc_of_json j)
      with Json_conv.Json_conv_error err ->
	raise (Invalid_rpc_decl (i, "rpc", (Json_conv.string_of_error err)))
    else if (Json_conv.is_object_field_present obj "endpoint_name") then
      try Rpc_decl.Rpc_endpoint (endpoint_of_json j)
      with Json_conv.Json_conv_error err ->
	raise (Invalid_rpc_decl (i, "endpoint", (Json_conv.string_of_error err)))
    else
      raise (Unknown_rpc_decl (i, j))

let _ =
  let input, endpoints = parse_args () in
    try
      let jdecls = parse_file input in
      let decls = List.map process_jdecl jdecls in
      let spec = Rpc_decl.spec_with_decls decls in
      let gen = List.map (fun (n, outf) ->
                            let e = try Rpc_decl.get_endpoint_by_name spec n
                            with Not_found -> raise (Undefined_endpoint n)
                            in e, outf
                         ) endpoints in
        Codegen.generate spec gen;
        exit 0
    with e ->
      print_exception e;
      exit 1
