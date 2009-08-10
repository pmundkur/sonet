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

(* Protocol constants. *)

val little_endian : char
val big_endian : char

val method_call_msg : int
val method_return_msg : int
val error_msg : int
val signal_msg : int

val no_reply_expected_flag : int
val no_auto_start_flag : int

val protocol_version : char

val hdr_array_type : Dbus_type.t

val path_hdr : char
val path_hdr_type : Dbus_type.t

val interface_hdr : char
val interface_hdr_type : Dbus_type.t

val member_hdr : char
val member_hdr_type : Dbus_type.t

val error_name_hdr : char
val error_name_hdr_type : Dbus_type.t

val reply_serial_hdr : char
val reply_serial_hdr_type : Dbus_type.t

val destination_hdr : char
val destination_hdr_type : Dbus_type.t

val sender_hdr : char
val sender_hdr_type : Dbus_type.t

val signature_hdr : char
val signature_hdr_type : Dbus_type.t

val all_headers : (char * Dbus_type.t * Dbus_message.header) list
