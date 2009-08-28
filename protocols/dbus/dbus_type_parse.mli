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

type inv_reason =
  | Inv_non_boolean
  | Inv_string of Dbus_value.string_error
  | Inv_object_path of Dbus_value.object_path_error
  | Inv_signature of Dbus_type.sig_error
  | Inv_variant_signature of Dbus_value.t
  | Inv_array_length

type error =
  | Insufficient_data of Dbus_type.t * (* remaining bytes *) int * (* needed bytes *) int
  | Invalid_value of Dbus_type.t * inv_reason

val error_message : error -> string

exception Parse_error of error

type context

val init_context : Dbus_type.endian -> string -> offset:int -> length:int -> context

val num_parsed_bytes : context -> int
val num_remaining_bytes : context -> int
val get_parse_data : context -> string * (* offset *) int * (* length *) int
val num_alignment_bytes : context -> align:int -> int

val append_bytes : context -> string -> offset:int -> length:int -> context
val advance : context -> int -> context
val rewind : context -> int -> context
val check_and_align_context : context -> align:int -> size:int -> Dbus_type.t -> context

val take_byte : ?dtype:Dbus_type.t -> context -> char * context
val parse_byte : context -> Dbus_value.t * context

val take_int16 : context -> int * context
val take_uint16 : context -> int * context
val parse_int16 : context -> Dbus_value.t * context
val parse_uint16 : context -> Dbus_value.t * context

val take_uint32 : ?dtype:Dbus_type.t -> context -> int64 * context
val parse_int32 : context -> Dbus_value.t * context
val parse_uint32 : context -> Dbus_value.t * context

val parse_boolean : context -> Dbus_value.t * context

val take_uint64 : ?dtype:Dbus_type.t -> context -> int64 * context
val parse_int64 : context -> Dbus_value.t * context
val parse_uint64 : context -> Dbus_value.t * context

val take_string : ?dtype:Dbus_type.t -> context -> string * context
val parse_string : context -> Dbus_value.t * context

val parse_object_path : context -> Dbus_value.t * context

val parse_signature : context -> Dbus_value.t * context

val parse_double : context -> Dbus_value.t * context

val parse_complete_type : Dbus_type.t -> context -> Dbus_value.t * context
val parse_type_list : Dbus_type.t list -> context -> Dbus_value.t list * context

val enable_debug_log : unit -> unit
val disable_debug_log : unit -> unit
