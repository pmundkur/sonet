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

(* Protocol constants *)

module T = Dbus_type
module M = Dbus_message

let little_endian = 'l'
let big_endian = 'B'

let method_call_msg = 1
let method_return_msg = 2
let error_msg = 3
let signal_msg = 4

let no_reply_expected_flag = 0x1
let no_auto_start_flag = 0x2

let protocol_version = Char.chr 1

let hdr_array_type = T.T_array (T.T_struct [ T.T_base T.B_byte; T.T_variant ])

let path_hdr = Char.chr 1
let path_hdr_type = T.T_base T.B_object_path

let interface_hdr = Char.chr 2
let interface_hdr_type = T.T_base T.B_string

let member_hdr = Char.chr 3
let member_hdr_type = T.T_base T.B_string

let error_name_hdr = Char.chr 4
let error_name_hdr_type = T.T_base T.B_string

let reply_serial_hdr = Char.chr 5
let reply_serial_hdr_type = T.T_base T.B_uint32

let destination_hdr = Char.chr 6
let destination_hdr_type = T.T_base T.B_string

let sender_hdr = Char.chr 7
let sender_hdr_type = T.T_base T.B_string

let signature_hdr = Char.chr 8
let signature_hdr_type = T.T_base T.B_signature

let all_headers = [
  path_hdr, path_hdr_type, M.Hdr_path;
  interface_hdr, interface_hdr_type, M.Hdr_interface;
  member_hdr, member_hdr_type, M.Hdr_member;
  error_name_hdr, error_name_hdr_type, M.Hdr_error_name;
  reply_serial_hdr, reply_serial_hdr_type, M.Hdr_reply_serial;
  destination_hdr, destination_hdr_type, M.Hdr_destination;
  sender_hdr, sender_hdr_type, M.Hdr_sender;
  signature_hdr, signature_hdr_type, M.Hdr_signature;
]

