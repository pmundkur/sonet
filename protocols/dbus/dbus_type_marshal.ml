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

let verbose = ref false

let dbg fmt =
  let logger s = if !verbose then Printf.printf "%s\n%!" s
  in Printf.ksprintf logger fmt

let enable_debug_log () =
  verbose := true

type error =
  | Signature_too_long
  | Insufficient_space of T.t

let error_message = function
  | Signature_too_long   -> "Signature too long"
  | Insufficient_space t -> Printf.sprintf "Insufficient space for %s" (T.to_string t)

exception Marshal_error of error
let raise_error e =
  raise (Marshal_error e)

let rec compute_marshaled_size ~stream_offset t v =
  V.type_check t v;
  let padding = T.get_padding ~offset:stream_offset ~align:(T.alignment_of t) in
  let size =
    match t, v with
      | _, V.V_byte _         -> padding + 1
      | _, V.V_boolean _      -> padding + 4
      | _, V.V_int16 _
      | _, V.V_uint16 _       -> padding + 2
      | _, V.V_int32 _
      | _, V.V_uint32 _       -> padding + 4
      | _, V.V_int64 _
      | _, V.V_uint64 _
      | _, V.V_double _       -> padding + 8
      | _, V.V_string s
      | _, V.V_object_path s  -> padding + 4 + String.length s + 1
      | _, V.V_signature tl ->
          let s = T.signature_of_types tl in
            if String.length s > 255
            then raise_error Signature_too_long
            else padding + 1 + String.length s + 1
      | T.T_array te, V.V_array va ->
          let offset = stream_offset + padding + 4 in
          let end_offset =
            Array.fold_left (fun stream_offset ve ->
                               stream_offset + compute_marshaled_size ~stream_offset te ve
                            ) offset va
          in end_offset - stream_offset
      | T.T_struct tl, V.V_struct vl ->
          let offset = stream_offset + padding in
          let end_offset =
            List.fold_left2 (fun stream_offset t v ->
                               stream_offset + compute_marshaled_size ~stream_offset t v
                            ) offset tl vl
          in end_offset - stream_offset
      | T.T_variant, V.V_variant (t, v) ->
          let offset = stream_offset + padding in
          let offset = (offset +
                          (compute_marshaled_size ~stream_offset:offset
                             (T.T_base T.B_signature) (V.V_signature [ t ]))) in
          let offset = offset + compute_marshaled_size ~stream_offset:offset t v
          in offset - stream_offset
      | _ ->
          (* We should never reach here after the type_check in the first line. *)
          assert false
  in
    dbg " [compute_marshaled_size: ofs=%d size=%d type=%s val=%s"
      stream_offset size (T.to_string t) (V.to_string v);
    size

let compute_payload_marshaled_size ~stream_offset tlist vlist =
  V.type_check_args tlist vlist;
  let offset =
    List.fold_left2 (fun stream_offset t v ->
                       stream_offset + compute_marshaled_size ~stream_offset t v
                    ) stream_offset tlist vlist
  in offset - stream_offset

type context = {
  endian : T.endian;
  buffer : string;
  length : int;
  starting_stream_offset : int;
  starting_buffer_offset : int;
  current_buffer_offset  : int;
}

let init_context ~stream_offset endian buffer  ~offset ~length =
  {
    endian = endian;
    buffer = buffer;
    length = length;
    starting_stream_offset = stream_offset;
    starting_buffer_offset = offset;
    current_buffer_offset  = offset;
  }

let get_marshaled_size ctxt =
  ctxt.current_buffer_offset - ctxt.starting_buffer_offset

let get_current_stream_offset ctxt =
  ctxt.starting_stream_offset + (get_marshaled_size ctxt)

let advance ctxt nbytes =
  assert (ctxt.length >= nbytes);
  { ctxt with
      current_buffer_offset = ctxt.current_buffer_offset + nbytes;
      length = ctxt.length - nbytes;
  }

let check_and_align_context ctxt ~align ~size dtype =
  let padding = T.get_padding ~offset:(get_current_stream_offset ctxt) ~align in
    if ctxt.length < padding + size then
      raise_error (Insufficient_space dtype);
    advance ctxt padding

let put_byte ?(dtype=T.T_base T.B_byte) ctxt b =
  let align = T.alignment_of (T.T_base T.B_byte) in
  let ctxt = check_and_align_context ctxt ~align ~size:1 dtype in
    ctxt.buffer.[ctxt.current_buffer_offset] <- b;
    advance ctxt 1

let marshal_byte ctxt v =
  put_byte ctxt (C.to_byte v)

(* TODO: check int64 (and other!) sanity! *)

let put_u16 ?(dtype=T.T_base T.B_uint16) ctxt (b0, b1) =
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:2 dtype in
    ctxt.buffer.[ctxt.current_buffer_offset] <- b0;
    ctxt.buffer.[ctxt.current_buffer_offset + 1] <- b1;
    advance ctxt 2

let from_uint16 endian i =
  let b0, b1 = Char.chr (i land 0xff), Char.chr ((i lsr 8) land 0xff) in
    match endian with
      | T.Little_endian -> b0, b1
      | T.Big_endian -> b1, b0

let from_int16 endian i =
  let sign = i < 0 in
  let b0 = Char.chr (i land 0xff) in
  let b1 = (i lsr 8) land 0xff in
  let b1 = Char.chr (if sign then b1 lor 0x80 else b1) in
    match endian with
      | T.Little_endian -> b0, b1
      | T.Big_endian -> b1, b0

let marshal_int16 ctxt v =
  let i = C.to_int16 v in
    put_u16 ~dtype:(T.T_base T.B_int16) ctxt (from_int16 ctxt.endian i)

let marshal_uint16 ctxt v =
  let i = C.to_uint16 v in
    put_u16 ~dtype:(T.T_base T.B_uint16) ctxt (from_uint16 ctxt.endian i)

let put_u32 ?(dtype=T.T_base T.B_uint32) ctxt (b0, b1, b2, b3) =
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:4 dtype in
    ctxt.buffer.[ctxt.current_buffer_offset] <- b0;
    ctxt.buffer.[ctxt.current_buffer_offset + 1] <- b1;
    ctxt.buffer.[ctxt.current_buffer_offset + 2] <- b2;
    ctxt.buffer.[ctxt.current_buffer_offset + 3] <- b3;
    advance ctxt 4

let from_int32 endian i =
  let b0 = Char.chr (Int32.to_int (Int32.logand i 0xffl)) in
  let b1 = Char.chr (Int32.to_int
                       (Int32.logand (Int32.shift_right_logical i  8) 0xffl)) in
  let b2 = Char.chr (Int32.to_int
                       (Int32.logand (Int32.shift_right_logical i 16) 0xffl)) in
  let b3 = Char.chr (Int32.to_int
                       (Int32.logand (Int32.shift_right_logical i 24) 0xffl)) in
    match endian with
      | T.Little_endian -> b0, b1, b2, b3
      | T.Big_endian -> b3, b2, b1, b0

let from_uint32 endian i =
  let b0 = Char.chr (Int64.to_int (Int64.logand i 0xffL)) in
  let b1 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i  8) 0xffL)) in
  let b2 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 16) 0xffL)) in
  let b3 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 24) 0xffL)) in
    match endian with
      | T.Little_endian -> b0, b1, b2, b3
      | T.Big_endian -> b3, b2, b1, b0

let marshal_int32 ctxt v =
  let i = C.to_int32 v in
    put_u32 ~dtype:(T.T_base T.B_uint32) ctxt (from_int32 ctxt.endian i)

let marshal_uint32 ctxt v =
  let i = C.to_uint32 v in
    put_u32 ~dtype:(T.T_base T.B_uint32) ctxt (from_uint32 ctxt.endian i)

let marshal_boolean ctxt v =
  let b = if C.to_boolean v then 1 else 0 in
    put_u32 ~dtype:(T.T_base T.B_boolean) ctxt (from_int32 ctxt.endian
                                                  (Int32.of_int b))

let put_u64 ?(dtype=T.T_base T.B_uint64) ctxt (b0, b1, b2, b3, b4, b5, b6, b7) =
  let align = T.alignment_of dtype in
  let ctxt = check_and_align_context ctxt ~align ~size:8 dtype in
    ctxt.buffer.[ctxt.current_buffer_offset] <- b0;
    ctxt.buffer.[ctxt.current_buffer_offset + 1] <- b1;
    ctxt.buffer.[ctxt.current_buffer_offset + 2] <- b2;
    ctxt.buffer.[ctxt.current_buffer_offset + 3] <- b3;
    ctxt.buffer.[ctxt.current_buffer_offset + 4] <- b4;
    ctxt.buffer.[ctxt.current_buffer_offset + 5] <- b5;
    ctxt.buffer.[ctxt.current_buffer_offset + 6] <- b6;
    ctxt.buffer.[ctxt.current_buffer_offset + 7] <- b7;
    advance ctxt 8

let from_int64 endian i =
  let b0 = Char.chr (Int64.to_int (Int64.logand i 0xffL)) in
  let b1 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i  8) 0xffL)) in
  let b2 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 16) 0xffL)) in
  let b3 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 24) 0xffL)) in
  let b4 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 32) 0xffL)) in
  let b5 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 40) 0xffL)) in
  let b6 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 48) 0xffL)) in
  let b7 = Char.chr (Int64.to_int
                       (Int64.logand (Int64.shift_right_logical i 56) 0xffL)) in
    match endian with
      | T.Little_endian -> b0, b1, b2, b3, b4, b5, b6, b7
      | T.Big_endian -> b7, b6, b5, b4, b3, b2, b1, b0

let from_uint64 = from_int64

let marshal_int64 ctxt v =
  let i = C.to_int64 v in
    put_u64 ~dtype:(T.T_base T.B_int64) ctxt (from_int64 ctxt.endian i)

let marshal_uint64 ctxt v =
  let i = C.to_uint64 v in
    put_u64 ~dtype:(T.T_base T.B_uint64) ctxt (from_uint64 ctxt.endian i)

let marshal_double ctxt v =
  (* TODO: via C or Oo.magic? *)
  let d = C.to_double v in
    ignore d;
    ctxt

let put_string ?(dtype=T.T_base T.B_string) ctxt s =
  let slen = String.length s in
    (* TODO: fix so that we can check_context for appropriate size *)
  let ctxt = (put_u32 ~dtype:(T.T_base T.B_string) ctxt
                (from_uint32 ctxt.endian (Int64.of_int slen))) in
    String.blit s 0 ctxt.buffer ctxt.current_buffer_offset slen;
    let ctxt = advance ctxt slen in
      put_byte ctxt '\x00'

let marshal_string ctxt v =
  let s = C.to_string v in
    put_string ctxt s

let marshal_object_path ctxt v =
  let o = C.to_object_path v in
    put_string ctxt o

let marshal_signature ctxt v =
  let tl = C.to_signature v in
  let signature = T.signature_of_types tl in
  let slen = String.length signature in
  let ctxt = (check_and_align_context ctxt ~align:1 ~size:(1 + slen + 1)
                (T.T_base T.B_signature)) in
  let ctxt = put_byte ctxt (Char.chr slen) in
    String.blit signature 0 ctxt.buffer ctxt.current_buffer_offset slen;
    let ctxt = advance ctxt slen in
      put_byte ctxt '\x00'

let get_base_marshaler = function
  | T.B_byte        -> marshal_byte
  | T.B_boolean     -> marshal_boolean
  | T.B_int16       -> marshal_int16
  | T.B_uint16      -> marshal_uint16
  | T.B_int32       -> marshal_int32
  | T.B_uint32      -> marshal_uint32
  | T.B_int64       -> marshal_int64
  | T.B_uint64      -> marshal_uint64
  | T.B_double      -> marshal_double
  | T.B_string      -> marshal_string
  | T.B_object_path -> marshal_object_path
  | T.B_signature   -> marshal_signature

let rec marshal_complete_type ctxt t v =
  V.type_check t v;
  let ctxt' =
  match t, v with
    | T.T_base b, _ ->
        (get_base_marshaler b) ctxt v
    | T.T_array te, V.V_array va ->
        (* "A UINT32 giving the length of the array data in bytes,
           followed by alignment padding to the alignment boundary of the
           array element type, followed by each array element. The array
           length is from the end of the alignment padding to the end of
           the last element, i.e. it does not include the padding after
           the length, or any padding after the last element."

           We need to compute the size of the array in bytes; to do
           that using compute_marshaled_size, we need to compute the
           stream_offset at which the array data will be marshaled.
        *)
        let stream_offset = get_current_stream_offset ctxt in
          (* Compute the padding before we put down the array size. *)
        let pre_size_padding  = T.get_padding ~offset:stream_offset ~align:4 in
          (* Compute the padding after the array size, but before the
             array data. *)
        let pre_data_padding_offset = stream_offset + pre_size_padding + 4 in
        let pre_data_padding = (T.get_padding ~offset:pre_data_padding_offset
                                  ~align:(T.alignment_of te)) in
        let data_start_offset = pre_data_padding_offset + pre_data_padding in
        let end_offset =
          Array.fold_left (fun stream_offset ve ->
                             stream_offset + compute_marshaled_size ~stream_offset te ve
                          ) data_start_offset va in
        let array_size = end_offset - data_start_offset in
        let ctxt = put_u32 ~dtype:t ctxt (from_int32 ctxt.endian (Int32.of_int array_size)) in
          Array.fold_left (fun ctxt v ->
                             marshal_complete_type ctxt te v
                          ) ctxt va
    | T.T_struct tl, V.V_struct vl ->
        let ctxt = check_and_align_context ctxt ~align:(T.alignment_of t) ~size:0 t in
          List.fold_left2 (fun ctxt t v ->
                             marshal_complete_type ctxt t v
                          ) ctxt tl vl
    | T.T_variant, V.V_variant (t, v) ->
        let ctxt = marshal_signature ctxt (V.V_signature [ t ]) in
          marshal_complete_type ctxt t v
    | _ ->
        (* We should never reach here after the type_check in the first line. *)
        assert false
  in
    dbg " [marshal_complete_type: ofs=%d size=%d type=%s value=%s"
      (get_current_stream_offset ctxt)
      ((get_marshaled_size ctxt') - (get_marshaled_size ctxt))
      (T.to_string t) (V.to_string v);
    ctxt'

let marshal_payload ctxt tlist vlist =
  V.type_check_args tlist vlist;
  List.fold_left2 (fun ctxt t v ->
                     marshal_complete_type ctxt t v
                  ) ctxt tlist vlist
