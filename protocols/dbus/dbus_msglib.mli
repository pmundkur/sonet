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
  | Unknown_request_name_reply of int
  | Unknown_release_name_reply of int
  | Unknown_service_start_reply of int
exception Message_error of error

val make_peer_ping_msg : destination:string -> serial:int64 -> Dbus_message.t
val make_peer_get_machine_id_msg : destination:string -> serial:int64 -> Dbus_message.t

val make_introspect_msg : destination:string -> serial:int64 -> Dbus_message.t

val make_property_get_msg :
  destination:string -> serial:int64 -> interface:string -> property:string ->
  Dbus_message.t
val make_property_set_msg :
  destination:string -> serial:int64 -> interface:string -> property:string ->
  val_type:Dbus_type.t -> value:Dbus_value.t ->
  Dbus_message.t
val make_property_getall_msg :
  destination:string -> serial:int64 -> interface:string -> Dbus_message.t

type name_flag =
  | Name_allow_replacement
  | Name_do_not_queue
  | Name_replace_existing
type request_name_reply =
  | Name_reply_primary_owner
  | Name_reply_in_queue
  | Name_exists
  | Name_already_owner
val make_request_name_msg :
  serial:int64 -> name:string -> flags:name_flag list -> Dbus_message.t
val request_name_reply_of_int : int -> request_name_reply

type release_name_reply =
  | Release_name_reply_released
  | Release_name_reply_non_existent
  | Release_name_reply_not_owner
val make_release_name_msg : serial:int64 -> name:string -> Dbus_message.t
val release_name_reply_of_int : int -> release_name_reply

val make_hello_msg : serial:int64 -> Dbus_message.t
val make_list_names_msg : serial:int64 -> Dbus_message.t
val make_list_activatable_names_msg : serial:int64 -> Dbus_message.t
val make_name_has_owner_msg : serial:int64 -> name:string -> Dbus_message.t

type service_start_reply =
  | Service_start_reply_success
  | Service_start_reply_already_running
val make_start_service_by_name_msg : serial:int64 -> name:string -> Dbus_message.t
val service_start_reply_of_int : int -> service_start_reply

val make_get_name_owner_msg : serial:int64 -> name:string -> Dbus_message.t
val make_get_connection_unix_user_msg :
  serial:int64 -> connection_name:string -> Dbus_message.t
val make_add_match_msg : serial:int64 -> filter:string -> Dbus_message.t
val make_remove_match_msg : serial:int64 -> filter:string -> Dbus_message.t
val make_get_id_msg : serial:int64 -> Dbus_message.t
