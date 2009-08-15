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

module T = Dbus_type
module V = Dbus_value
module C = Dbus_conv
module P = Dbus_type_marshal
module M = Dbus_message

exception Header_not_found of M.header

let pack_headers hdrs =
  let lookup_header_info hdr =
    let rec helper = function
      | (h_code, h_type, h_id) :: hlist when h_id = hdr -> h_code, h_type
      | _ :: hlist -> helper hlist
      | [] -> assert false (* we currently don't support custom headers. *)
    in helper Protocol.all_headers in
  let structs =
    List.map (fun (h, (ht, hv)) ->
                V.type_check ht hv;
                let h_code, h_type = lookup_header_info h in
                  assert (h_type = ht);
                  V.V_struct [ V.V_byte h_code; V.V_variant (ht, hv) ]
             ) hdrs
  in V.V_array (Array.of_list structs)

let pack_flags flags =
  let no_reply_expected =
    if List.mem M.Msg_flag_no_reply_expected flags
    then Protocol.no_reply_expected_flag
    else 0x0 in
  let no_auto_start =
    if List.mem M.Msg_flag_no_auto_start flags
    then Protocol.no_auto_start_flag
    else 0x0
  in Char.chr (no_reply_expected lor no_auto_start)

let compute_marshaled_size stream_offset m =
  (* The signature of the header is:
     BYTE, BYTE, BYTE, BYTE, UINT32, UINT32, ARRAY of STRUCT of (BYTE,VARIANT)
     or, showing padding,
     BYTE, BYTE, BYTE, BYTE, padding-4, UINT32, UINT32, ARRAY.length (UINT32), padding-8
  *)
  let offset = stream_offset + 4 * 1 in
  let offset = offset + (T.get_padding ~offset ~align:4) in
  let offset = offset + 2 * 4 in
  let offset = offset + (P.compute_marshaled_size ~stream_offset:offset
                           Protocol.hdr_array_type (pack_headers (M.get_headers m))) in
  (* "The length of the header must be a multiple of 8, allowing the
     body to begin on an 8-byte boundary when storing the entire message
     in a single buffer. If the header does not naturally end on an
     8-byte boundary up to 7 bytes of nul-initialized alignment padding
     must be added." *)
  let offset = offset + (T.get_padding ~offset ~align:8) in
  let signature, payload = M.get_signature m, M.get_payload m in
  let offset = offset + P.compute_payload_marshaled_size ~stream_offset:offset signature payload
  in offset - stream_offset

let get_message_type_code = function
  | M.Msg_method_call _   -> Char.chr Protocol.method_call_msg
  | M.Msg_method_return _ -> Char.chr Protocol.method_return_msg
  | M.Msg_error _         -> Char.chr Protocol.error_msg
  | M.Msg_signal _        -> Char.chr Protocol.signal_msg

let get_endian_code = function
  | T.Little_endian       -> Protocol.little_endian
  | T.Big_endian          -> Protocol.big_endian

let marshal_message ~stream_offset endian buffer ~offset ~length m =
  buffer.[offset] <- get_endian_code endian;
  let stream_offset, offset, length = stream_offset + 1, offset + 1, length - 1 in
  let ctxt = P.init_context ~stream_offset endian buffer ~offset ~length in
  let ctxt = P.marshal_byte ctxt (V.V_byte (get_message_type_code m)) in
  let ctxt = P.marshal_byte ctxt (V.V_byte (pack_flags (M.get_flags m))) in
  let ctxt = P.marshal_byte ctxt (V.V_byte Protocol.protocol_version) in
  let signature, payload = M.get_signature m, M.get_payload m in
  let stream_offset = stream_offset + P.get_marshaled_size ctxt in
  let payload_length = P.compute_payload_marshaled_size ~stream_offset signature payload in
  let ctxt = P.marshal_uint32 ctxt (V.V_uint32 (Int64.of_int payload_length)) in
  let ctxt = P.marshal_uint32 ctxt (V.V_uint32 (M.get_serial m)) in
  let ctxt = (P.marshal_complete_type ctxt Protocol.hdr_array_type
                (pack_headers (M.get_headers m))) in
  let ctxt = P.marshal_payload ctxt signature payload in
    P.get_marshaled_size ctxt
