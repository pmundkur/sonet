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

type parse_state

val init_parse_state: unit -> parse_state

type parse_result =
  | Json_value of Json.t * (* number of consumed bytes *) int
  | Json_parse_incomplete of parse_state

val parse: parse_state -> string -> parse_result
val parse_substring: parse_state -> string -> (* offset *) int -> (* length *) int -> parse_result

val finish_parse: parse_state -> Json.t option

val num_chars_parsed: parse_state -> int

(* first integer argument is the line number *)
type error =
  | Unexpected_char of int * char * (* json type *) string
  | Invalid_value of int * (* value *) string * (* json type *) string
  | Invalid_leading_zero of int * string
  | Unterminated_value of int * string
  | Internal_error of int * string
val string_of_error: error -> string

exception Parse_error of error

(* convenience functions *)
val of_string: string -> Json.t
val of_substring: string -> (* offset *) int -> (* length *) int -> Json.t
