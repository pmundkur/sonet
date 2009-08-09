(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Syntax
open Parser
open Lexing

let show_syntax_error e l =
  let loc = Printf.sprintf "%s at line %d, char %d"
    l.pos_fname l.pos_lnum (l.pos_cnum - l.pos_bol) in
  let msg =
    match e with
      | Illegal_character c -> Printf.sprintf "Illegal character %c" c
      | Invalid_ident s -> Printf.sprintf "Invalid/unsupported identifier %s" s
      | Unsupported_type_constructor s -> Printf.sprintf "Unsupported type constructor %s" s
      | Unmatched_comment -> Printf.sprintf "Unmatched comment"
      | Unterminated_comment -> Printf.sprintf "Unterminated comment"
  in
    Printf.eprintf "%s: %s\n" loc msg;
    exit 1

let show_parse_error lexbuf =
  let lxm = lexeme lexbuf in
  let loc = Printf.sprintf "%s at line %d, char %d"
    lexbuf.lex_curr_p.pos_fname lexbuf.lex_curr_p.pos_lnum
    (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) in
    (match lxm with
       | "" -> Printf.eprintf "%s: parsing error\n" loc
       | _  -> Printf.eprintf "%s: parsing error at \"%s\"\n" loc lxm);
    exit 1

let parse_file file =
  let f = open_in file in
  let lexbuf = Lexing.from_channel f in
    try
      Lexer.init lexbuf file;
      Parser.defn_list Lexer.main lexbuf
    with
      | Syntax_error (e, l) ->
          show_syntax_error e l
      | Parsing.Parse_error ->
          show_parse_error lexbuf

let default_output_filename f =
  let dir, base = Filename.dirname f, Filename.basename f in
  let stem = Filename.chop_extension base in
    Filename.concat dir (stem ^ "_json_conv.ml")

let () =
  Printexc.record_backtrace true;
  let input = ref "" in
  let output = ref "" in

  (* parse argv *)
  let larg = [
    ("-i", Arg.Set_string input, " input file");
    ("-o", Arg.Set_string output, " output file");
    ("-d", Arg.Set Codegen.verbose, " debug codegen logging");
  ] in
  let usage_msg = Printf.sprintf "%s -i <file> [-o <file>]" Sys.argv.(0) in
    Arg.parse larg (fun s -> ()) usage_msg;

    if !output = "" then output := default_output_filename !input;

    match !input with
      | "" -> Printf.printf "%s\n" usage_msg
      | file -> Codegen.generate (parse_file file) !output !input
