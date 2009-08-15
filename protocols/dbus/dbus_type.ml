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

type base =
  | B_byte          (* 'y' *)
  | B_boolean       (* 'b' *)
  | B_int16         (* 'n' *)
  | B_uint16        (* 'q' *)
  | B_int32         (* 'i' *)
  | B_uint32        (* 'u' *)
  | B_int64         (* 'x' *)
  | B_uint64        (* 't' *)
  | B_double        (* 'd' *)
  | B_string        (* 's' *)
  | B_object_path   (* 'o' *)
  | B_signature     (* 'g' *)

type t =
  | T_base of base
  | T_variant           (* 'v' *)
  | T_array of t        (* 'a' *)
  | T_struct of t list  (* 'r', '(' .. ')' *)

let rec to_string = function
  | T_base B_byte        -> "byte"
  | T_base B_boolean     -> "boolean"
  | T_base B_int16       -> "int16"
  | T_base B_uint16      -> "uint16"
  | T_base B_int32       -> "int32"
  | T_base B_uint32      -> "uint32"
  | T_base B_int64       -> "int64"
  | T_base B_uint64      -> "uint64"
  | T_base B_double      -> "double"
  | T_base B_string      -> "string"
  | T_base B_object_path -> "object_path"
  | T_base B_signature   -> "signature"
  | T_variant            -> "variant"
  | T_array t            -> "array(" ^ (to_string t) ^ ")"
  | T_struct tl          -> "struct(" ^ String.concat "," (List.map to_string tl) ^ ")"

let rec to_code = function
  | T_base B_byte        -> "y"
  | T_base B_boolean     -> "b"
  | T_base B_int16       -> "n"
  | T_base B_uint16      -> "q"
  | T_base B_int32       -> "i"
  | T_base B_uint32      -> "u"
  | T_base B_int64       -> "x"
  | T_base B_uint64      -> "t"
  | T_base B_double      -> "d"
  | T_base B_string      -> "s"
  | T_base B_object_path -> "o"
  | T_base B_signature   -> "g"
  | T_variant            -> "v"
  | T_array t            -> "a" ^ (to_code t)
  | T_struct tl          -> "(" ^ String.concat "" (List.map to_string tl) ^ ")"

let is_basic_type = function
  | T_base _ -> true
  | _        -> false

let is_container_type t = not (is_basic_type t)

let alignment_of = function
  | T_base B_byte        -> 1
  | T_base B_boolean     -> 4
  | T_base B_int16       -> 2
  | T_base B_uint16      -> 2
  | T_base B_int32       -> 4
  | T_base B_uint32      -> 4
  | T_base B_int64       -> 8
  | T_base B_uint64      -> 8
  | T_base B_double      -> 8
  | T_base B_string      -> 4
  | T_base B_object_path -> 4
  | T_base B_signature   -> 1
  | T_variant            -> 1
  | T_array _            -> 4
  | T_struct _           -> 8

let get_padding ~offset ~align =
  let ofs = offset mod align in
    match ofs with
      | 0 -> 0
      | _ -> align - ofs

type endian =
  | Little_endian
  | Big_endian

type sig_error =
  | Sig_incomplete
  | Sig_invalid of string
  | Sig_invalid_char of char

let sig_error_message = function
  | Sig_incomplete     -> "incomplete signature"
  | Sig_invalid s      -> Printf.sprintf "invalid signature '%s'" s
  | Sig_invalid_char c -> Printf.sprintf "invalid signature char '%c'" c

exception Invalid_signature of sig_error
let raise_sig_error se = raise (Invalid_signature se)

let as_dict_entry = function
  | T_struct [ k; v ] when is_basic_type k -> Some (k, v)
  | _ -> None

let is_valid_dict_entry t =
  as_dict_entry t <> None

let rec get_complete_type clist in_array =
  match clist with
    | [] ->
        raise_sig_error Sig_incomplete
    | 'y' :: rem ->
        (T_base B_byte), rem
    | 'b' :: rem ->
        (T_base B_boolean), rem
    | 'n' :: rem ->
        (T_base B_int16), rem
    | 'q' :: rem ->
        (T_base B_uint16), rem
    | 'i' :: rem ->
        (T_base B_int32), rem
    | 'u' :: rem ->
        (T_base B_uint32), rem
    | 'x' :: rem ->
        (T_base B_int64), rem
    | 't' :: rem ->
        (T_base B_uint64), rem
    | 'd' :: rem ->
        (T_base B_double), rem
    | 's' :: rem ->
        (T_base B_string), rem
    | 'o' :: rem ->
        (T_base B_object_path), rem
    | 'g' :: rem ->
        (T_base B_signature), rem
    | 'v' :: rem ->
	T_variant, rem
    | 'a' :: rem ->
	let t, rem = get_complete_type rem true in
	  (T_array t), rem
    | '(' :: rem ->
        get_struct_type rem
    | ('{' as c) :: rem ->
        if not in_array then
          raise_sig_error (Sig_invalid_char c)
        else
          let t, rem = get_struct_type rem in
            if is_valid_dict_entry t
            then (T_array t), rem
            else raise_sig_error (Sig_invalid "dict_entry not in array context")
    | c :: _ ->
        raise_sig_error (Sig_invalid_char c)

and get_struct_type clist =
  let rec helper acc = function
    | [] ->
	raise_sig_error Sig_incomplete
    | ')' :: rem ->
        acc, rem
    | clist ->
        let t, rem = get_complete_type clist false in
          helper (t :: acc) rem in
  let tl, rem = helper [] clist in
    if List.length tl = 0 then raise_sig_error (Sig_invalid "empty struct")
    else (T_struct (List.rev tl)), rem

let string_to_char_list s =
  let rec accum_list l idx =
    if idx = 0 then s.[0] :: l
    else accum_list (s.[idx] :: l) (idx - 1)
  in
    accum_list [] ((String.length s) - 1)

let signature_of_string s =
  let rec helper acc = function
    | [] -> acc
    | clist ->
        let t, rem = get_complete_type clist false in
          helper (t :: acc) rem
  in
    if String.length s > 255 then
      raise_sig_error (Sig_invalid "signature exceeds 255 bytes")
    else
      List.rev (helper [] (string_to_char_list s))

let signature_of_types tlist =
  let rec sig_one = function
    | T_base B_byte        -> "y"
    | T_base B_boolean     -> "b"
    | T_base B_int16       -> "n"
    | T_base B_uint16      -> "q"
    | T_base B_int32       -> "i"
    | T_base B_uint32      -> "u"
    | T_base B_int64       -> "x"
    | T_base B_uint64      -> "t"
    | T_base B_double      -> "d"
    | T_base B_string      -> "s"
    | T_base B_object_path -> "o"
    | T_base B_signature   -> "g"
    | T_variant            -> "v"
    | T_array t ->
        "a" ^ (match as_dict_entry t with
                 | Some (k, v) -> "{" ^ sig_one k ^ sig_one v ^ "}"
                 | _ -> sig_one t
              )
    | T_struct tl ->
        "(" ^ (String.concat "" (List.map sig_one tl)) ^ ")"
  in
    String.concat "" (List.map sig_one tlist)
