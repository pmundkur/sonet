(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2009      Prashanth Mundkur.                            *)
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

(* We use a universal type representation.  This breaks type-safety by
   allowing heterogenous arrays, which violate the D-Bus type system.
   We ensure safety by using the type_check function (in this module)
   wherever possible.  The use of automated converters instead of
   constructing values manually would also help.
*)

type t =
  | V_byte of char
  | V_boolean of bool
  | V_int16 of int
  | V_uint16 of int
  | V_int32 of int32
  | V_uint32 of int64
  | V_int64 of int64
  | V_uint64 of int64
  | V_double of float
  | V_string of string
  | V_object_path of string
  | V_signature of Dbus_type.t list
  | V_array of t array
  | V_struct of t list
  | V_variant of Dbus_type.t * t

val to_string : t -> string
val string_type_of : t -> string

(* Kinds of invalid errors. *)

type object_path_error =
  | OP_with_invalid_char
  | OP_with_consecutive_slashes
  | OP_with_non_slash_prefix
  | OP_is_slash_terminated

type string_error =
  | String_with_embedded_nul
  | String_not_nul_terminated

type type_check_error =
  | Type_mismatch of Dbus_type.t * t
  | Type_arg_length_mismatch of Dbus_type.t list * t list

type error =
  | Untyped_array
  | String_error of string_error
  | Object_path_error of object_path_error
  | Type_check_error of type_check_error

exception Invalid_value_error of error

val object_path_error_message : object_path_error -> string
val string_error_message : string_error -> string
val type_check_error_message : type_check_error -> string
val error_message : error -> string

(* String validity checker: raises Invalid_value_error String_error *)
val check_valid_string : string -> unit
val is_valid_string : string -> bool

(* Object_path validity checker: raises Invalid_value_error Object_path_error *)
val check_valid_object_path : string -> unit
val is_valid_object_path : string -> bool

(* Type checker: raises Invalid_value_error Type_mismatch. *)
val type_check : Dbus_type.t -> t -> unit
val type_check_args : Dbus_type.t list -> t list -> unit

(* Pretty printer *)
val pr_value : Format.formatter -> t -> unit
