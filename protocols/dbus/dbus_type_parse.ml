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

type inv_reason =
  | Inv_non_boolean
  | Inv_string of V.string_error
  | Inv_object_path of V.object_path_error
  | Inv_signature of T.sig_error
  | Inv_variant_signature of V.t
  | Inv_array_length

let inv_reason_message = function
  | Inv_non_boolean         -> "invalid boolean"
  | Inv_string e            -> Printf.sprintf "invalid string: %s" (V.string_error_message e)
  | Inv_object_path e       -> Printf.sprintf "invalid object-path: %s" (V.object_path_error_message e)
  | Inv_signature e         -> Printf.sprintf "invalid signature: %s" (T.sig_error_message e)
  | Inv_variant_signature v -> Printf.sprintf "invalid variant signature for %s" (V.to_string v)
  | Inv_array_length        -> "invalid array length"

type error =
  | Insufficient_data of T.t * (* remaining bytes *) int * (* needed bytes *) int
  | Invalid_value of T.t * inv_reason

let error_message = function
  | Insufficient_data (t, r, n) ->
      Printf.sprintf "insufficient data for type %s: %d bytes remaining, %d needed"
        (T.to_string t) r n
  | Invalid_value (t, r) ->
      Printf.sprintf "invalid %s value: %s" (T.to_string t) (inv_reason_message r)

exception Parse_error of error
let raise_error e =
  raise (Parse_error e)

type context = {
  endian : T.endian;
  buffer : string;
  offset : int;
  length : int;
  starting_offset : int;
}

let init_context endian buffer ~offset:offset ~length:length = {
  endian = endian;
  buffer = buffer;
  offset = offset;
  length = length;
  starting_offset = offset;
}

let num_parsed_bytes ctxt =
  ctxt.offset - ctxt.starting_offset

let num_remaining_bytes ctxt =
  ctxt.length

let num_alignment_bytes ctxt ~align =
  T.get_padding ~offset:ctxt.offset ~align

let append_bytes ctxt str ~offset:ofs ~length:len =
  { ctxt with
      buffer = ctxt.buffer ^ (String.sub str ofs len);
      length = ctxt.length + len;
  }

let advance ctxt nbytes =
  assert (ctxt.length >= nbytes);
  { ctxt with
      offset = ctxt.offset + nbytes;
      length = ctxt.length - nbytes;
  }

let rewind ctxt nbytes =
  assert (ctxt.offset >= nbytes);
  { ctxt with
      offset = ctxt.offset - nbytes;
      length = ctxt.length + nbytes;
  }

let check_and_align_context ctxt ~align ~size dtype =
  let padding = num_alignment_bytes ctxt ~align in
    if ctxt.length < padding + size then
      raise_error (Insufficient_data (dtype, ctxt.length, padding + size));
    advance ctxt padding

let take_byte ?(dtype=T.T_base T.B_byte) ctxt =
  let align = T.alignment_of (T.T_base T.B_byte) in
  let ctxt = check_and_align_context ctxt ~align ~size:1 dtype in
  let b = ctxt.buffer.[ctxt.offset] in
    b, advance ctxt 1

let parse_byte ctxt =
  let b, ctxt = take_byte ctxt in
    V.V_byte b, ctxt

let to_int16 endian b0 b1 =
  let i, sign = match endian with
    | T.Little_endian -> b0 + (b1 lsl 8), b1 lsr 7
    | T.Big_endian -> b1 + (b0 lsl 8), b0 lsr 7
  in if sign = 0 then i else (i land 0x7fff) - 0x8000

let to_uint16 endian b0 b1 =
  match endian with
    | T.Little_endian -> b0 + (b1 lsl 8)
    | T.Big_endian -> b1 + (b0 lsl 8)

let to_int32 endian q0 q1 =
  let module I = Int32 in
  let q0, q1 = I.of_int q0, I.of_int q1 in
    match endian with
      | T.Little_endian -> I.add q0 (I.shift_left q1 16)
      | T.Big_endian -> I.add q1 (I.shift_left q0 16)

let to_uint32 endian q0 q1 =
  let module I = Int64 in
  let q0, q1 = I.of_int q0, I.of_int q1 in
    match endian with
      | T.Little_endian -> I.add q0 (I.shift_left q1 16)
      | T.Big_endian -> I.add q1 (I.shift_left q0 16)

let to_uint64 endian u0 u1 =
  let module I = Int64 in
    match endian with
      | T.Little_endian -> I.add u0 (I.shift_left u1 32)
      | T.Big_endian -> I.add u1 (I.shift_left u0 32)

let take_i16 sign ctxt =
  let to_fn, dtype =
    if sign then to_int16, (T.T_base T.B_int16)
    else to_uint16, (T.T_base T.B_uint16) in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:2 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
    to_fn ctxt.endian b0 b1, advance ctxt 2

let take_int16 = take_i16 true
let take_uint16 = take_i16 false

let parse_int16 ctxt =
  let i, ctxt = take_int16 ctxt in
    V.V_int16 i, ctxt

let parse_uint16 ctxt =
  let i, ctxt = take_uint16 ctxt in
    V.V_uint16 i, ctxt

let take_uint32 ?(dtype=T.T_base T.B_uint32) ctxt =
  let align = T.alignment_of (T.T_base T.B_int32) in
  let ctxt = check_and_align_context ctxt ~align ~size:4 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    to_uint32 ctxt.endian q0 q1, advance ctxt 4

let parse_int32 ctxt =
  let dtype = T.T_base T.B_int32 in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:4 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
    V.V_int32 (to_int32 ctxt.endian q0 q1), advance ctxt 4

let parse_uint32 ctxt =
  let i, ctxt = take_uint32 ctxt in
    V.V_uint32 i, ctxt

let parse_boolean ctxt =
  let dtype = T.T_base T.B_boolean in
  let i, ctxt = take_uint32 ~dtype ctxt in
  let b =
    if i <> 0L && i <> 1L
    then raise_error (Invalid_value (dtype, Inv_non_boolean))
    else (if i = 0L then false else true)
  in
    V.V_boolean b, ctxt

(* TODO: check int64 (and other!) sanity! *)
let take_uint64 ?(dtype=T.T_base T.B_uint64) ctxt =
  let align = T.alignment_of (T.T_base T.B_int64) in
  let ctxt = check_and_align_context ctxt ~align ~size:8 dtype in
  let b0 = Char.code ctxt.buffer.[ctxt.offset] in
  let b1 = Char.code ctxt.buffer.[ctxt.offset + 1] in
  let b2 = Char.code ctxt.buffer.[ctxt.offset + 2] in
  let b3 = Char.code ctxt.buffer.[ctxt.offset + 3] in
  let b4 = Char.code ctxt.buffer.[ctxt.offset + 4] in
  let b5 = Char.code ctxt.buffer.[ctxt.offset + 5] in
  let b6 = Char.code ctxt.buffer.[ctxt.offset + 6] in
  let b7 = Char.code ctxt.buffer.[ctxt.offset + 7] in
  let q0 = to_uint16 ctxt.endian b0 b1 in
  let q1 = to_uint16 ctxt.endian b2 b3 in
  let q2 = to_uint16 ctxt.endian b4 b5 in
  let q3 = to_uint16 ctxt.endian b6 b7 in
  let u0 = to_uint32 ctxt.endian q0 q1 in
  let u1 = to_uint32 ctxt.endian q2 q3 in
    to_uint64 ctxt.endian u0 u1, advance ctxt 8

let parse_int64 ctxt =
  let i, ctxt = take_uint64 ~dtype:(T.T_base T.B_int64) ctxt in
    V.V_int64 i, ctxt

let parse_uint64 ctxt =
  let u, ctxt = take_uint64 ctxt in
    V.V_uint64 u, ctxt

let check_valid_string ?(dtype=T.T_base T.B_string) s =
  try V.check_valid_string s
  with V.Invalid_value_error (V.String_error e) ->
    raise_error (Invalid_value (dtype, Inv_string e))

let take_string ?(dtype=T.T_base T.B_string) ctxt =
  let len, ctxt = take_uint32 ~dtype ctxt in
  let len = Int64.to_int len in
    (* the below call is only to check the length, since we're already aligned. *)
  let ctxt = check_and_align_context ctxt ~align:1 ~size:(len + 1) dtype in
  let s = String.sub ctxt.buffer ctxt.offset len in
    check_valid_string ~dtype s;
    if ctxt.buffer.[ctxt.offset + len] <> '\x00' then
      raise_error (Invalid_value (dtype, Inv_string V.String_not_nul_terminated));
    s, advance ctxt (len + 1)

let parse_string ctxt =
  let s, ctxt = take_string ctxt in
    V.V_string s, ctxt

let check_valid_object_path ?(dtype=T.T_base T.B_object_path) s =
  try V.check_valid_object_path s
  with
    | V.Invalid_value_error (V.Object_path_error e) ->
        raise_error (Invalid_value (dtype, Inv_object_path e))

let parse_object_path ctxt =
  let dtype = T.T_base T.B_object_path in
  let s, ctxt = take_string ~dtype ctxt in
    check_valid_object_path s;
    V.V_object_path s, ctxt

let parse_signature ctxt =
  let dtype = T.T_base T.B_signature in
  let b, ctxt = take_byte ~dtype ctxt in
  let slen = Char.code b in
  let s = String.sub ctxt.buffer ctxt.offset slen in
  let signature = (try T.signature_of_string s
                   with T.Invalid_signature se ->
                     raise_error (Invalid_value (dtype, Inv_signature se))) in
    if ctxt.buffer.[ctxt.offset + slen] <> '\x00' then
      raise_error (Invalid_value (dtype, Inv_signature T.Sig_not_nul_terminated));
    V.V_signature signature, advance ctxt (slen + 1)

let parse_double ctxt =
  let dtype = T.T_base T.B_double in
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:8 dtype in
    (* TODO: Do it in C, or better yet, with some Oo.black magic. *)
    V.V_double 0.0, advance ctxt 8

let get_base_parser = function
  | T.B_byte ->         parse_byte
  | T.B_boolean ->      parse_boolean
  | T.B_int16 ->        parse_int16
  | T.B_uint16 ->       parse_uint16
  | T.B_int32 ->        parse_int32
  | T.B_uint32 ->       parse_uint32
  | T.B_int64 ->        parse_int64
  | T.B_uint64 ->       parse_uint64
  | T.B_double ->       parse_double
  | T.B_string ->       parse_string
  | T.B_object_path ->  parse_object_path
  | T.B_signature ->    parse_signature

let rec parse_complete_type dtype ctxt =
  let v, ctxt' =
    match dtype with
      | T.T_base b ->
          (get_base_parser b) ctxt
      | T.T_variant ->
          let s, ctxt = parse_signature ctxt in
          let tl = C.to_signature s in
          let t = (match tl with
                     | [ t ] -> t
                     | _     ->
                         raise_error (Invalid_value (dtype, Inv_variant_signature s))
                  ) in
          let v, ctxt = parse_complete_type t ctxt in
            V.V_variant (t, v), ctxt
      | T.T_array t ->
          (* "A UINT32 giving the length of the array data in bytes,
             followed by alignment padding to the alignment boundary of the
             array element type, followed by each array element. The array
             length is from the end of the alignment padding to the end of
             the last element, i.e. it does not include the padding after
             the length, or any padding after the last element."
          *)
          let len, ctxt = take_uint32 ~dtype ctxt in
          let len = Int64.to_int len in
          let align = T.alignment_of t in
          let ctxt = check_and_align_context ctxt ~align ~size:len dtype in
          let end_offset = ctxt.offset + len in
          let rec iter acc ctxt =
            if ctxt.offset < end_offset then
              let e, ctxt = parse_complete_type t ctxt in
                iter (e :: acc) ctxt
            else acc, ctxt in
          let alist, ctxt = iter [] ctxt in
            if ctxt.offset > end_offset then raise_error (Invalid_value (dtype, Inv_array_length))
            else V.V_array (Array.of_list (List.rev alist)), ctxt
      | T.T_struct tl ->
          let align = T.alignment_of dtype in
            (* the below call only performs alignment; the length check
               is performed during the loop. *)
          let ctxt = check_and_align_context ctxt ~align ~size:0 dtype in
          let vl, ctxt = parse_type_list tl ctxt in
            V.V_struct vl, ctxt
  in
    V.type_check dtype v;
    v, ctxt'

and parse_type_list dtypes ctxt =
  let vl, ctxt =
    List.fold_left (fun (vl, ctxt) t ->
                      let v, ctxt = parse_complete_type t ctxt in
                        (v :: vl), ctxt
                   ) ([], ctxt) dtypes in
    List.rev vl, ctxt
