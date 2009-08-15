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
module P = Dbus_type_parse
module M = Dbus_message

type fixed_header = {
  fixed_buffer : string;
  start_offset : int;
  mutable offset: int;
}

type context = {
  buffer : Buffer.t;
  mutable type_context : P.context;
  mutable msg_type : M.msg_type;
  mutable payload_length : int;
  mutable flags : M.flag list;
  mutable protocol_version : char;
  mutable serial : int64;
  mutable bytes_remaining : int;
  mutable headers : (M.header * (T.t * V.t)) list;
  mutable signature : T.t list;
  mutable payload : V.t list;
}

type state =
  | In_fixed_header of fixed_header
  | In_headers of context
  | In_payload of context

type parse_result =
  | Parse_incomplete of state
  | Parse_result of M.t * (* number of unconsumed bytes *) int

type error =
  | Invalid_endian
  | Unknown_msg_type of int
  | Unexpected_header_type of M.header * (* received *) T.t * (* expected *) T.t
  | Missing_signature_header_for_payload
  | Missing_required_header of M.msg_type * M.header

let error_message = function
  | Invalid_endian ->
      "invalid endian byte"
  | Unknown_msg_type i ->
      Printf.sprintf "unknown message type %d" i
  | Unexpected_header_type (h, r, e) ->
      Printf.sprintf "header %s had a value of type %s, but a %s was expected"
        (M.header_to_string h) (T.to_string r) (T.to_string e)
  | Missing_signature_header_for_payload ->
      "no signature for payload"
  | Missing_required_header (mt, h) ->
      Printf.sprintf "header %s is required for a %s"
        (M.header_to_string h) (M.msg_type_to_string mt)

exception Parse_error of error
let raise_error e =
  raise (Parse_error e)

(* The signature of the header is:
   BYTE, BYTE, BYTE, BYTE, UINT32, UINT32, ARRAY of STRUCT of (BYTE,VARIANT)
   or, showing padding,
   BYTE, BYTE, BYTE, BYTE, padding-4, UINT32, UINT32, ARRAY.length (UINT32), padding-8
*)
let init_state start_offset =
  (* Compute the number of bytes to read for the fixed length header,
     taking into account the alignments of the elements of the fixed
     header. *)
  let start_offset = start_offset mod 8 in
  let length = start_offset + 4 * 1 in
  let length = length + (T.get_padding ~offset:length ~align:4) in
  let length = length + 2 * 4 + 4 in
  let length = length + (T.get_padding ~offset:length ~align:8) in
  let buffer = String.make length '\x00' in
    In_fixed_header {
      fixed_buffer = buffer;
      start_offset = start_offset;
      offset = start_offset;
    }

let init_context type_context msg_type payload_length flags protocol_version
    serial bytes_remaining =
  {
    buffer = Buffer.create bytes_remaining;
    type_context = type_context;
    msg_type = msg_type;
    payload_length = payload_length;
    flags = flags;
    protocol_version = protocol_version;
    serial = serial;
    bytes_remaining = bytes_remaining;
    headers = [];
    signature = [];
    payload = [];
  }

let parse_flags flags =
  let flags = Char.code flags in
  let with_no_reply flist =
    if (flags land Protocol.no_reply_expected_flag
        = Protocol.no_reply_expected_flag)
    then M.Msg_flag_no_reply_expected :: flist
    else flist in
  let with_no_auto_start flist =
    if (flags land Protocol.no_auto_start_flag
        = Protocol.no_auto_start_flag)
    then M.Msg_flag_no_auto_start :: flist
    else flist
  in
    with_no_reply (with_no_auto_start [])

let parse_msg_type mtype =
  let mtype = Char.code mtype in
    if mtype = Protocol.method_call_msg
    then M.Msg_type_method_call
    else if mtype = Protocol.method_return_msg
    then M.Msg_type_method_return
    else if mtype = Protocol.error_msg
    then M.Msg_type_error
    else if mtype = Protocol.signal_msg
    then M.Msg_type_signal
    else raise_error (Unknown_msg_type mtype)

let lookup_required_header msg_type hdr ctxt =
  try List.assoc hdr ctxt.headers
  with Not_found -> raise_error (Missing_required_header (msg_type, hdr))

let lookup_optional_header hdr ctxt =
  try Some (List.assoc hdr ctxt.headers)
  with Not_found -> None

let lookup_optional_string_header hdr ctxt =
  match lookup_optional_header hdr ctxt with
    | Some (_, string_val) -> Some (C.to_string string_val)
    | None -> None

(* For more informative error handling during header processing below,
   we should also check that the header variant-values have the
   expected types; for now, we rely on C.to_* throwing
   (less-informative) conversion exceptions.
*)

let make_method_call_msg ctxt =
  let lookup_required = lookup_required_header M.Msg_type_method_call in
  let _, path_val = lookup_required M.Hdr_path ctxt in
  let _, member_val = lookup_required M.Hdr_member ctxt in
  let path = C.to_object_path path_val in
  let member = C.to_string member_val in
  let interface = lookup_optional_string_header M.Hdr_interface ctxt in
  let destination = lookup_optional_string_header M.Hdr_destination ctxt in
  let sender = lookup_optional_string_header M.Hdr_sender ctxt in
    M.Msg_method_call {
      M.method_call_flags = ctxt.flags;
      M.method_call_serial = ctxt.serial;
      M.method_call_path = path;
      M.method_call_member = member;
      M.method_call_interface = interface;
      M.method_call_destination = destination;
      M.method_call_sender = sender;
      M.method_call_signature = ctxt.signature;
      M.method_call_payload = ctxt.payload;
    }

let make_method_return_msg ctxt =
  let lookup_required = lookup_required_header M.Msg_type_method_return in
  let _, reply_serial_val = lookup_required M.Hdr_reply_serial ctxt in
  let reply_serial = C.to_uint32 reply_serial_val in
  let destination = lookup_optional_string_header M.Hdr_destination ctxt in
  let sender = lookup_optional_string_header M.Hdr_sender ctxt in
    M.Msg_method_return {
      M.method_return_flags = ctxt.flags;
      M.method_return_serial = ctxt.serial;
      M.method_return_reply_serial = reply_serial;
      M.method_return_destination = destination;
      M.method_return_sender = sender;
      M.method_return_signature = ctxt.signature;
      M.method_return_payload = ctxt.payload;
    }

let make_error_msg ctxt =
  let lookup_required = lookup_required_header M.Msg_type_error in
  let _, error_name_val = lookup_required M.Hdr_path ctxt in
  let error_name = C.to_string error_name_val in
  let _, reply_serial_val = lookup_required M.Hdr_reply_serial ctxt in
  let reply_serial = C.to_uint32 reply_serial_val in
  let destination = lookup_optional_string_header M.Hdr_destination ctxt in
  let sender = lookup_optional_string_header M.Hdr_sender ctxt in
    M.Msg_error {
      M.error_flags = ctxt.flags;
      M.error_serial = ctxt.serial;
      M.error_name = error_name;
      M.error_reply_serial = reply_serial;
      M.error_destination = destination;
      M.error_sender = sender;
      M.error_signature = ctxt.signature;
      M.error_payload = ctxt.payload;
    }

let make_signal_msg ctxt =
  let lookup_required = lookup_required_header M.Msg_type_signal in
  let _, path_val = lookup_required M.Hdr_path ctxt in
  let _, interface_val = lookup_required M.Hdr_interface ctxt in
  let _, member_val = lookup_required M.Hdr_member ctxt in
  let path = C.to_object_path path_val in
  let interface = C.to_string interface_val in
  let member = C.to_string member_val in
  let destination = lookup_optional_string_header M.Hdr_destination ctxt in
  let sender = lookup_optional_string_header M.Hdr_sender ctxt in
    M.Msg_signal {
      M.signal_flags = ctxt.flags;
      M.signal_serial = ctxt.serial;
      M.signal_path = path;
      M.signal_interface = interface;
      M.signal_member = member;
      M.signal_destination = destination;
      M.signal_sender = sender;
      M.signal_signature = ctxt.signature;
      M.signal_payload = ctxt.payload;
    }

let make_message ctxt =
  match ctxt.msg_type with
    | M.Msg_type_method_call    -> make_method_call_msg ctxt
    | M.Msg_type_method_return  -> make_method_return_msg ctxt
    | M.Msg_type_error          -> make_error_msg ctxt
    | M.Msg_type_signal         -> make_signal_msg ctxt

(* See the documentation for init_state. *)
let process_fixed_header fh =
  let endian = fh.fixed_buffer.[fh.start_offset] in
  let endian =
    if endian = Protocol.little_endian then T.Little_endian
    else if endian = Protocol.big_endian then T.Big_endian
    else raise_error Invalid_endian in
  let tctxt = (P.init_context endian fh.fixed_buffer
                 ~offset:(fh.start_offset + 1)
                 ~length:((String.length fh.fixed_buffer) - fh.start_offset - 1)) in
  let msg_type, tctxt = P.take_byte tctxt in
  let msg_type = parse_msg_type msg_type in
  let flags, tctxt = P.take_byte tctxt in
  let flags = parse_flags flags in
  let protocol_version, tctxt = P.take_byte tctxt in
  let tctxt = P.check_and_align_context tctxt ~align:4 ~size:(3 * 4) (T.T_base T.B_uint32) in
  let payload_length, tctxt = P.take_uint32 tctxt in
  let serial, tctxt = P.take_uint32 tctxt in
    (* The remaining bytes are the bytes in the header array data.  We
       won't update our type_parsing context 'tctxt' now; since we
       need it to parse the array once we get the array data. *)
  let bytes_remaining, _ = P.take_uint32 tctxt in
    init_context tctxt msg_type (Int64.to_int payload_length) flags
      protocol_version serial (Int64.to_int bytes_remaining)

let unpack_headers hdr_array =
  (* hdr_array is an array of byte-indexed dict_entries (structs); we
     want to get a byte-indexed list of pairs for use with
     List.assoc. *)
  let hl = Array.to_list (C.to_array hdr_array) in
  let hl = List.map (fun hs -> C.to_struct hs) hl in
  let assoc = List.map (fun hv ->
                          let hb = List.nth hv 0 in
                          let hv = List.nth hv 1 in
                            (C.to_byte hb, C.to_variant hv)
                       ) hl in
  (* For now, we just look up the known standard headers; we could
     store the unknown headers too in the message if we really need
     them. *)
  let headers =
    List.map (fun (hdr_code, hdr_type, hdr) ->
                try
                  let ht, hv = List.assoc hdr_code assoc in
                    if ht <> hdr_type
                      (* ht is an unexpected type for standard header *)
                    then raise_error (Unexpected_header_type (hdr, ht, hdr_type))
                    else [ hdr, (hdr_type, hv) ]
                with Not_found -> []
             ) Protocol.all_headers
  in List.concat headers

let process_headers ctxt =
  let tctxt = (P.append_bytes ctxt.type_context
                 (Buffer.contents ctxt.buffer) 0 (Buffer.length ctxt.buffer)) in
  (* At this point, the parsing context is where process_fixed_header
     left it, i.e. ready to parse the array. *)
  let hdr_array, tctxt = P.parse_complete_type Protocol.hdr_array_type tctxt in
  let headers = unpack_headers hdr_array in
    (* The header "array length does not include any padding after the
       last element"; however, "the header ends after its alignment
       padding to an 8-boundary".  So, the latter alignment still
       remains, and we perform it now. *)
  let tctxt = P.check_and_align_context tctxt ~align:8 ~size:0 Protocol.hdr_array_type in
    ctxt.type_context <- tctxt;
    ctxt.headers <- headers;
    (* We now prepare to read in the payload, if any. *)
    ctxt.bytes_remaining <- ctxt.payload_length

let process_payload ctxt =
  (* We have a payload, so we should have a signature header. *)
  let _, sigval =
    try List.assoc M.Hdr_signature ctxt.headers
    with Not_found -> raise_error Missing_signature_header_for_payload in
    (* For more informative error handling, we should also check that
       the signature header has a signature-type; for now, we rely on
       C.to_signature throwing a (less-informative) conversion
       exception. *)
  let signature = C.to_signature sigval in
    (* The parsing context has already been adjusted for us in
       process_headers, so we're ready to parse the payload. *)
  let payload, tctxt = P.parse_type_list signature ctxt.type_context in
    ctxt.signature <- signature;
    ctxt.payload <- payload;
    ctxt.type_context <- tctxt

let rec parse_substring state str ofs len =
  match state with
    | In_fixed_header fh ->
        let fh_bytes_remaining = (String.length fh.fixed_buffer) - fh.offset in
        let bytes_to_consume = min len fh_bytes_remaining in
          String.blit str ofs fh.fixed_buffer fh.offset bytes_to_consume;
          fh.offset <- fh.offset + bytes_to_consume;
          if bytes_to_consume < fh_bytes_remaining
            then Parse_incomplete (In_fixed_header fh)
            else begin
              let ctxt = process_fixed_header fh in
                if ctxt.bytes_remaining = 0
                then Parse_result (make_message ctxt, len - bytes_to_consume)
                else (parse_substring (In_headers ctxt)
                        str (ofs + bytes_to_consume) (len - bytes_to_consume))
            end
    | In_headers ctxt ->
        let bytes_to_consume = min len ctxt.bytes_remaining in
          Buffer.add_substring ctxt.buffer str ofs bytes_to_consume;
          ctxt.bytes_remaining <- ctxt.bytes_remaining - bytes_to_consume;
          if ctxt.bytes_remaining > 0
          then Parse_incomplete (In_headers ctxt)
          else begin
            process_headers ctxt;
            (* We need to check again to see if we expect a payload. *)
            if ctxt.bytes_remaining = 0
            then Parse_result (make_message ctxt, len - bytes_to_consume)
            else (parse_substring (In_payload ctxt)
                    str (ofs + bytes_to_consume) (len - bytes_to_consume))
          end
    | In_payload ctxt ->
        let bytes_to_consume = min len ctxt.bytes_remaining in
        let tctxt = P.append_bytes ctxt.type_context str ofs bytes_to_consume in
          ctxt.type_context <- tctxt;
          ctxt.bytes_remaining <- ctxt.bytes_remaining - bytes_to_consume;
          if ctxt.bytes_remaining > 0
          then Parse_incomplete (In_payload ctxt)
          else begin
            process_payload ctxt;
            Parse_result (make_message ctxt, len - bytes_to_consume)
          end
