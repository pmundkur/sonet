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
val string_of_json: ?permissive:bool -> Json.t -> string
val string_to_json: string -> Json.t

val int_of_json: ?permissive:bool -> Json.t -> int
val int_to_json: int -> Json.t

val int64_of_json: ?permissive:bool -> Json.t -> int64
val int64_to_json: int64 -> Json.t

val float_of_json: ?permissive:bool -> Json.t -> float
val float_to_json: float -> Json.t

val bool_of_json: ?permissive:bool -> Json.t -> bool
val bool_to_json: bool -> Json.t

(* utilities used by generated code *)
val raise_unexpected_json_type: string -> string -> 'a
val raise_short_array: int -> int -> 'a
val raise_unknown_constructor: string -> string -> 'a
val check_array_with_length: 'a array -> int -> unit
val get_variant_constructor: Json.t -> string * Json.t array
val get_array: Json.t -> Json.t array
val get_array_elem: Json.t array -> int -> Json.t
val get_list: Json.t -> Json.t list

type object_table
val get_object_table: Json.t -> object_table
val get_object_field: object_table -> string -> Json.t
val get_optional_object_field: object_table -> string -> Json.t
val is_object_field_present: object_table -> string -> bool
