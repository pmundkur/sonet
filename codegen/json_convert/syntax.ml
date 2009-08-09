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

type syntax_error =
  | Illegal_character of char
  | Invalid_ident of string
  | Unsupported_type_constructor of string
  | Unmatched_comment
  | Unterminated_comment

exception Syntax_error of syntax_error * Lexing.position

type base_type =
  | B_string
  | B_int
  | B_int64
  | B_float
  | B_bool
  | B_ident of string

type complex_type =
  | C_base of base_type
  | C_option of complex_type
  | C_list of complex_type
  | C_array of complex_type
  | C_tuple of complex_type list
  | C_record of (string * complex_type) list
  | C_variant of constr_decl list

and constr_decl =
  | CD_tuple of string * complex_type list

type type_defn =
  | T_manifest of (string * complex_type) list
  | T_abstract of string

type defn =
  | Type_defn of type_defn
  | Pragma of char list

