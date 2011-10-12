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

(* conversion errors *)
type error =
  | Unexpected_json_type of (* rcvd *) string * (* expected *) string
  | Array_length of (* rcvd *) int * (* expected *) int
  | Unknown_constructor of (* type *) string * (* constructor *) string
  | Missing_object_field of string
val string_of_error: error -> string

exception Json_conv_error of error


(* conversion routines for base types *)
val to_string: ?permissive:bool -> Json.t -> string
val of_string: string -> Json.t

val to_int: ?permissive:bool -> Json.t -> int
val of_int: int -> Json.t

val to_int64: ?permissive:bool -> Json.t -> int64
val of_int64: int64 -> Json.t

val to_float: ?permissive:bool -> Json.t -> float
val of_float: float -> Json.t

val to_bool: ?permissive:bool -> Json.t -> bool
val of_bool: bool -> Json.t

(* utilities used by generated code *)
val raise_unexpected_json_type: string -> string -> 'a
val raise_short_array: int -> int -> 'a
val raise_unknown_constructor: string -> string -> 'a
val check_array_with_length: 'a array -> int -> unit
val get_variant_constructor: Json.t -> string * Json.t array
val to_array: Json.t -> Json.t array
val array_elem: Json.t array -> int -> Json.t
val to_list: Json.t -> Json.t list

type object_table
val to_object_table: Json.t -> object_table
val object_table_to_list: object_table -> (string * Json.t) list
val object_field: object_table -> string -> Json.t
val optional_object_field: object_table -> string -> Json.t
val has_object_field: object_table -> string -> bool
