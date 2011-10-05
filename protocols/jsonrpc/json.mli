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

type t =
  | Null
  | Undefined
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Object of (string * t) array
  | Array of t array

val string_of_type: t -> string

val to_fct_pretty: t -> (string -> unit) -> unit
val to_string: t -> string

val is_null: t -> bool
val is_bool: t -> bool
val is_int:  t -> bool
val is_float: t -> bool
val is_string: t -> bool
val is_object: t -> bool
val is_array: t -> bool
val is_scalar: t -> bool
