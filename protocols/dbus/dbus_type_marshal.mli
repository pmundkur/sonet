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
  | Signature_too_long
  | Insufficient_space of Dbus_type.t

exception Marshal_error of error

val error_message : error -> string

type context

val init_context : Dbus_type.endian -> string -> start_offset:int -> offset:int -> length:int -> context
val get_marshaled_size : context -> int

val compute_marshaled_size : offset:int -> Dbus_type.t -> Dbus_value.t -> int
val compute_payload_marshaled_size : Dbus_type.t list -> Dbus_value.t list -> int

val advance : context -> int -> context

val marshal_byte : context -> Dbus_value.t -> context
val marshal_uint32 : context -> Dbus_value.t -> context

val marshal_complete_type : context -> Dbus_type.t -> Dbus_value.t -> context
val marshal_payload : context -> Dbus_type.t list -> Dbus_value.t list -> context

val enable_debug_log : unit -> unit
val disable_debug_log : unit -> unit
