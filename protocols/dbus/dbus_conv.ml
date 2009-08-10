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

type error =
  | Unexpected_dbus_type of (* rcvd *) string * (* expected *) string
  | Integer_out_of_range
  | Invalid_string of string
  | Invalid_object_path of string

exception Dbus_conv of error
let raise_error e =
  raise (Dbus_conv e)

(* Note that in the below to_* functions (Dbus.t -> Ocaml.t), only a
   single unpacking of variant arguments is done (the recursive call
   leaves ?unpack_variants defaulting to false.
*)

let rec to_byte ?(unpack_variants=false) = function
  | V.V_byte c -> c
  | V.V_variant (T.T_base T.B_byte, v)
      when unpack_variants -> to_byte v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "byte"))

let of_byte c = V.V_byte c

let rec to_boolean ?(unpack_variants=false) = function
  | V.V_boolean b -> b
  | V.V_variant (T.T_base T.B_boolean, v)
      when unpack_variants -> to_boolean v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "boolean"))

let of_boolean b = V.V_boolean b

let rec to_int16 ?(unpack_variants=false) = function
  | V.V_int16 i -> i
  | V.V_variant (T.T_base T.B_int16, v)
      when unpack_variants -> to_int16 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int16"))

let of_int16 i =
  if i >= -32768 && i < 32768
  then V.V_int16 i
  else raise_error Integer_out_of_range

let rec to_uint16 ?(unpack_variants=false) = function
  | V.V_uint16 i -> i
  | V.V_variant (T.T_base T.B_uint16, v)
      when unpack_variants -> to_uint16 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint16"))

let of_uint16 i =
  if i land 0xffff = i
  then V.V_int16 i
  else raise_error Integer_out_of_range

let rec to_int32 ?(unpack_variants=false) = function
  | V.V_int32 i -> i
  | V.V_variant (T.T_base T.B_int32, v)
      when unpack_variants -> to_int32 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int32"))

let of_int32 i = V.V_int32 i

let rec to_uint32 ?(unpack_variants=false) = function
  | V.V_uint32 i -> i
  | V.V_variant (T.T_base T.B_uint32, v)
      when unpack_variants -> to_uint32 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint32"))

let of_uint32 i =
  if Int64.logand i 0xffffffffL = i
  then V.V_uint32 i
  else raise_error Integer_out_of_range

let rec to_int64 ?(unpack_variants=false) = function
  | V.V_int64 i -> i
  | V.V_variant (T.T_base T.B_int64, v)
      when unpack_variants -> to_int64 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "int64"))

let of_int64 i = V.V_int64 i

let rec to_uint64 ?(unpack_variants=false) = function
  | V.V_uint64 i -> i
  | V.V_variant (T.T_base T.B_uint64, v)
      when unpack_variants -> to_uint64 v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "uint64"))

(* TODO: fix signedness check *)
let of_uint64 i = V.V_uint64 i

let rec to_double ?(unpack_variants=false) = function
  | V.V_double d -> d
  | V.V_variant (T.T_base T.B_double, v)
      when unpack_variants -> to_double v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "double"))

let of_double d = V.V_double d

let rec to_string ?(unpack_variants=false) = function
  | V.V_string s -> s
  | V.V_variant (T.T_base T.B_string, v)
      when unpack_variants -> to_string v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "string"))

let of_string s =
  if V.is_valid_string s
  then V.V_string s
  else raise_error (Invalid_string s)

let rec to_object_path ?(unpack_variants=false) = function
  | V.V_object_path o -> o
  | V.V_variant (T.T_base T.B_object_path, v)
      when unpack_variants -> to_object_path v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "object_path"))

let of_object_path o =
  if V.is_valid_object_path o
  then V.V_object_path o
  else raise_error (Invalid_object_path o)

let rec to_signature ?(unpack_variants=false) = function
  | V.V_signature s -> s
  | V.V_variant (T.T_base T.B_signature, v)
      when unpack_variants -> to_signature v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "signature"))

let of_signature tl = V.V_signature tl

let rec to_array ?(unpack_variants=false) = function
  | V.V_array a -> a
  | V.V_variant (T.T_array _, v)
      when unpack_variants -> to_array v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "array"))

let of_array a = V.V_array a

let rec to_struct ?(unpack_variants=false) = function
  | V.V_struct s -> s
  | V.V_variant (T.T_struct _, v)
      when unpack_variants -> to_struct v
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "struct"))

let of_struct s = V.V_struct s

let to_variant = function
  | V.V_variant (t, v) -> (t, v)
  | v -> raise_error (Unexpected_dbus_type ((V.string_type_of v), "variant"))

let of_variant (t, v) = V.V_variant (t, v)
