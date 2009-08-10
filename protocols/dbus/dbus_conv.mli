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

type error =
  | Unexpected_dbus_type of string * string
  | Integer_out_of_range
  | Invalid_string of string
  | Invalid_object_path of string
exception Dbus_conv of error

val to_byte : ?unpack_variants:bool -> Dbus_value.t -> char
val of_byte : char -> Dbus_value.t

val to_boolean : ?unpack_variants:bool -> Dbus_value.t -> bool
val of_boolean : bool -> Dbus_value.t

val to_int16 : ?unpack_variants:bool -> Dbus_value.t -> int
val of_int16 : int -> Dbus_value.t

val to_uint16 : ?unpack_variants:bool -> Dbus_value.t -> int
val of_uint16 : int -> Dbus_value.t

val to_int32 : ?unpack_variants:bool -> Dbus_value.t -> int32
val of_int32 : int32 -> Dbus_value.t

val to_uint32 : ?unpack_variants:bool -> Dbus_value.t -> int64
val of_uint32 : int64 -> Dbus_value.t

val to_int64 : ?unpack_variants:bool -> Dbus_value.t -> int64
val of_int64 : int64 -> Dbus_value.t

val to_uint64 : ?unpack_variants:bool -> Dbus_value.t -> int64
val of_uint64 : int64 -> Dbus_value.t

val to_double : ?unpack_variants:bool -> Dbus_value.t -> float
val of_double : float -> Dbus_value.t

val to_string : ?unpack_variants:bool -> Dbus_value.t -> string
val of_string : string -> Dbus_value.t

val to_object_path : ?unpack_variants:bool -> Dbus_value.t -> string
val of_object_path : string -> Dbus_value.t

val to_signature : ?unpack_variants:bool -> Dbus_value.t -> Dbus_type.t list
val of_signature : Dbus_type.t list -> Dbus_value.t

val to_array : ?unpack_variants:bool -> Dbus_value.t -> Dbus_value.t array
val of_array : Dbus_value.t array -> Dbus_value.t

val to_struct : ?unpack_variants:bool -> Dbus_value.t -> Dbus_value.t list
val of_struct : Dbus_value.t list -> Dbus_value.t

val to_variant : Dbus_value.t -> Dbus_type.t * Dbus_value.t
val of_variant : Dbus_type.t * Dbus_value.t -> Dbus_value.t
