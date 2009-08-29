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
  | Invalid_endian
  | Unknown_msg_type of int
  | Unexpected_header_type of Dbus_message.header * (* received *) Dbus_type.t * (* expected *) Dbus_type.t
  | Missing_signature_header_for_payload
  | Missing_required_header of Dbus_message.msg_type * Dbus_message.header

val error_message : error -> string

exception Parse_error of error

type state

val init_state : unit -> state

type parse_result =
  | Parse_incomplete of state
  | Parse_result of Dbus_message.t * (* unconsumed bytes *) int

val parse_substring : state -> string -> int -> int -> parse_result

val enable_debug_log : unit -> unit
val disable_debug_log : unit -> unit

val enable_data_trace : unit -> unit
val disable_data_trace : unit -> unit
