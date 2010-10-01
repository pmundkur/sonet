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

module M = Dbus_message
module T = Dbus_type
module V = Dbus_value

type error =
  | Unknown_request_name_reply of int
  | Unknown_release_name_reply of int
  | Unknown_service_start_reply of int

exception Message_error of error
let raise_error e =
  raise (Message_error e)

let make_peer_ping_msg ~destination ~serial =
  M.method_call
    ~serial
    ~destination
    ~interface:"org.freedesktop.DBus.Peer"
    ~member:"Ping"
    ~signature:[]
    []

let make_peer_get_machine_id_msg ~destination ~serial =
  M.method_call
    ~serial
    ~destination
    ~interface:"org.freedesktop.DBus.Peer"
    ~member:"GetMachineId"
    ~signature:[]
    []

let make_introspect_msg ~destination ~serial =
  M.method_call
    ~serial
    ~destination
    ~interface:"org.freedesktop.DBus.Introspectable"
    ~member:"Introspect"
    ~signature:[]
    []

let make_property_get_msg ~destination ~serial ~interface ~property =
  let signature = [ (T.T_base T.B_string); (T.T_base T.B_string) ] in
  let payload = [ (V.V_string interface); (V.V_string property) ] in
  M.method_call
    ~serial
    ~destination
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"Get"
    ~signature
    payload

let make_property_set_msg ~destination ~serial ~interface ~property
    ~val_type ~value =
  let signature = [ T.T_base T.B_string; T.T_base T.B_string; T.T_variant ] in
  let payload = [ V.V_string interface;
                  V.V_string property;
                  V.V_variant (val_type, value) ] in
    M.method_call
      ~serial
      ~destination
      ~interface:"org.freedesktop.DBus.Properties"
      ~member:"Set"
      ~signature
      payload

let make_property_getall_msg ~destination ~serial ~interface =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string interface ] in
    M.method_call
      ~serial
      ~destination
      ~interface:"org.freedesktop.DBus.Properties"
      ~member:"GetAll"
      ~signature
      payload

type name_flag =
  | Name_allow_replacement
  | Name_do_not_queue
  | Name_replace_existing

let name_flag_to_int = function
  | Name_allow_replacement  -> 0x1
  | Name_replace_existing   -> 0x2
  | Name_do_not_queue       -> 0x4

type request_name_reply =
  | Name_reply_primary_owner
  | Name_reply_in_queue
  | Name_exists
  | Name_already_owner

let request_name_reply_of_int = function
  | 1 -> Name_reply_primary_owner
  | 2 -> Name_reply_in_queue
  | 3 -> Name_exists
  | 4 -> Name_already_owner
  | r -> raise_error (Unknown_request_name_reply r)

let make_request_name_msg ~serial ~name ~flags =
  let flags = List.map name_flag_to_int flags in
  let flags = List.fold_left (fun acc f -> acc lor f) 0x0 flags in
  let signature = [ T.T_base T.B_string; T.T_base T.B_uint32 ] in
  let payload = [ V.V_string name; V.V_uint32 (Int64.of_int flags) ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"RequestName"
      ~signature
      payload

let make_release_name_msg ~serial ~name =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string name ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"ReleaseName"
      ~signature
      payload

type release_name_reply =
  | Release_name_reply_released
  | Release_name_reply_non_existent
  | Release_name_reply_not_owner

let release_name_reply_of_int = function
  | 1 -> Release_name_reply_released
  | 2 -> Release_name_reply_non_existent
  | 3 -> Release_name_reply_not_owner
  | r -> raise_error (Unknown_release_name_reply r)

let make_hello_msg ~serial =
  M.method_call
    ~serial
    ~destination:"org.freedesktop.DBus"
    ~interface:"org.freedesktop.DBus"
    ~member:"Hello"
    ~signature:[]
    []

let make_list_names_msg ~serial =
  M.method_call
    ~serial
    ~destination:"org.freedesktop.DBus"
    ~interface:"org.freedesktop.DBus"
    ~member:"ListNames"
    ~signature:[]
    []

let make_list_activatable_names_msg ~serial =
  M.method_call
    ~serial
    ~destination:"org.freedesktop.DBus"
    ~interface:"org.freedesktop.DBus"
    ~member:"ListActivatableNames"
    ~signature:[]
    []

let make_name_has_owner_msg ~serial ~name =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string name ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"NameHasOwner"
      ~signature
      payload

type service_start_reply =
  | Service_start_reply_success
  | Service_start_reply_already_running

let service_start_reply_of_int = function
  | 1 -> Service_start_reply_success
  | 2 -> Service_start_reply_already_running
  | r -> raise_error (Unknown_service_start_reply r)

let make_start_service_by_name_msg ~serial ~name =
  let signature = [ T.T_base T.B_string; T.T_base T.B_uint32 ] in
  let payload = [ V.V_string name; V.V_uint32 (Int64.of_int 0) ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"StartServiceByName"
      ~signature
      payload

let make_get_name_owner_msg ~serial ~name =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string name ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"GetNameOwner"
      ~signature
      payload

let make_get_connection_unix_user_msg ~serial ~connection_name =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string connection_name ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"GetConnectionUnixUser"
      ~signature
      payload

let make_add_match_msg ~serial ~filter =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string filter ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"AddMatch"
      ~signature
      payload

let make_remove_match_msg ~serial ~filter =
  let signature = [ T.T_base T.B_string ] in
  let payload = [ V.V_string filter ] in
    M.method_call
      ~serial
      ~destination:"org.freedesktop.DBus"
      ~interface:"org.freedesktop.DBus"
      ~member:"RemoveMatch"
      ~signature
      payload

let make_get_id_msg ~serial =
  M.method_call
    ~serial
    ~destination:"org.freedesktop.DBus"
    ~interface:"org.freedesktop.DBus"
    ~member:"GetId"
    ~signature:[]
    []
