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

{

open Lexing
open Syntax
open Parser

let comment_depth = ref 0
let comment_start = ref dummy_pos

let line_num = ref 0

let init lexbuf fname =
  lexbuf.lex_curr_p <- { pos_fname = fname;
                         pos_lnum = 1;
                         pos_bol = 0;
                         pos_cnum = 0 }

let raise_syntax_error e loc =
  raise (Syntax_error (e, loc))

let cur_pragma = ref ([] : char list)

let get_pragma () =
  let p = !cur_pragma in
    cur_pragma := [];
    p
}

let letter = ['A'-'Z' 'a'-'z']

(* The handling of '.' is a bit of a hack for now; not sure if it's
   really needed. *)
let ident_first = letter | '_'
let ident_others  = letter | ['0'-'9'] | '_' | '\'' | '.'
let ident = ident_first ident_others*

rule main = parse
| [' ' '\009' '\012' '\r']+     { main lexbuf }

| ['\n']        { new_line lexbuf; main lexbuf}

| "*)"          { raise_syntax_error Unmatched_comment (lexeme_start_p lexbuf) }

| "(*** json-pragma: "
                { pragma lexbuf; PRAGMA (get_pragma ()) }

| "(*"          { comment_depth := 1; comment_start := lexeme_start_p lexbuf;
                  comment lexbuf; main lexbuf }

| eof           { EOF }
| "="           { EQUAL }
| "*"           { STAR }
| ";"           { SEMI }
| ";;"          { SEMISEMI }
| ":"           { COLON }
| "|"           { BAR }

| "{"           { LBRACE }
| "}"           { RBRACE }
| "("           { LPAREN }
| ")"           { RPAREN }
| "["           { LBRACK }
| "]"           { RBRACK }

| "type"        { TYPE }
| "and"         { AND }
| "mutable"     { MUTABLE }
| "of"          { OF }

(* general identifiers.  we could handle the '.' here. *)
| ident
    { let str = lexeme lexbuf in
        match String.get str 0 with
          | 'A' .. 'Z' -> if String.contains str '.' then QIDENT str else UIDENT str
          | 'a' .. 'z' -> LIDENT str
          | _ ->          raise_syntax_error (Invalid_ident str) (lexeme_start_p lexbuf)
        }
| _     { raise_syntax_error (Illegal_character (lexeme_char lexbuf 0)) (lexeme_start_p lexbuf) }


and comment = parse
| "(*"          { incr comment_depth; comment lexbuf }
| "*)"          { decr comment_depth; if !comment_depth > 0 then comment lexbuf }
| ['\n']        { new_line lexbuf; comment lexbuf }
| eof           { raise_syntax_error Unterminated_comment !comment_start }
| _             { comment lexbuf }

and pragma = parse
| " ***)"       { (* done *) }
| _             { cur_pragma := (lexeme_char lexbuf 0) :: !cur_pragma; pragma lexbuf }
