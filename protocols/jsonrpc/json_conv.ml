(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License (version
 * 2.1 only) as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type error =
  | Unexpected_json_type of (* rcvd *) string * (* expected *) string
  | Array_length of (* rcvd *) int * (* expected *) int
  | Unknown_constructor of (* type *) string * (* constructor *) string
  | Missing_object_field of string

let string_of_error = function
  | Unexpected_json_type (r, e) ->
      Printf.sprintf "type %s received when %s was expected" r e
  | Array_length (r, e) ->
      Printf.sprintf "array length %d received when %d was expected" r e
  | Unknown_constructor (t, c) ->
      Printf.sprintf "unknown constructor %s received for type %s" c t
  | Missing_object_field f ->
      Printf.sprintf "missing object field %s" f

exception Json_conv_error of error

let raise_unexpected_json_type typ exp =
  raise (Json_conv_error (Unexpected_json_type (typ, exp)))

let raise_short_array len exp =
  raise (Json_conv_error (Array_length (len, exp)))

let raise_unknown_constructor typ cons =
  raise (Json_conv_error (Unknown_constructor (typ, cons)))

let raise_missing_object_field field =
  raise (Json_conv_error (Missing_object_field field))

let to_string ?(permissive=false) j =
  let strict = function
    | Json.String s -> s
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "string" in
  let lenient = function
    | Json.Null     -> ""
    | Json.String s -> s
    | Json.Array a  ->
        if Array.length a = 0 then
          raise_unexpected_json_type "array" "string"
        else
          strict a.(0)
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "string" in
    if not permissive then strict j else lenient j

let of_string s = Json.String s

let to_int ?(permissive=false) j =
  let strict = function
    | Json.Int i    -> Int64.to_int i
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "int" in
  let lenient = function
    | Json.Null     -> 0
    | Json.Bool b   -> if b then 1 else 0
    | Json.Int i    -> Int64.to_int i
    | Json.Float f  -> int_of_float f
    | Json.Array a  ->
        if Array.length a = 0 then
          raise_unexpected_json_type "array" "int"
        else
          strict a.(0)
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "int" in
    if not permissive then strict j else lenient j

let of_int i = Json.Int (Int64.of_int i)

let to_int64 ?(permissive=false) j =
  let strict = function
    | Json.Int i    -> i
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "int64" in
  let lenient = function
    | Json.Null     -> 0L
    | Json.Bool b   -> if b then 1L else 0L
    | Json.Int i    -> i
    | Json.Float f  -> Int64.of_float f
    | Json.Array a  ->
        if Array.length a = 0 then
          raise_unexpected_json_type "array" "int64"
        else
          strict a.(0)
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "int64" in
    if not permissive then strict j else lenient j

let of_int64 i = Json.Int i

let to_float ?(permissive=false) j =
  let strict = function
    | Json.Float f  -> f
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "float" in
  let lenient = function
    | Json.Null     -> 0.0
    | Json.Bool b   -> if b then 1.0 else 0.0
    | Json.Int i    -> Int64.to_float i
    | Json.Float f  -> f
    | Json.Array a  ->
        if Array.length a = 0 then
          raise_unexpected_json_type "array" "float"
        else
          strict a.(0)
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "float" in
    if not permissive then strict j else lenient j

let of_float f = Json.Float f

let to_bool  ?(permissive=false) j =
  let strict = function
    | Json.Bool b   -> b
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "bool" in
  let lenient = function
    | Json.Null     -> false
    | Json.Bool b   -> b
    | Json.Int i    -> i <> 0L
    | Json.Float f  -> f <> 0.0
    | Json.Array a  ->
        if Array.length a = 0 then
          raise_unexpected_json_type "array" "bool"
        else
          strict a.(0)
    | _             -> raise_unexpected_json_type (Json.string_of_type j) "bool" in
    if not permissive then strict j else lenient j

let of_bool b = Json.Bool b


(* utilities *)

let check_array_with_length arr minlen =
  let alen = Array.length arr in
    if alen < minlen then
      raise_short_array minlen alen

let get_variant_constructor j =
  match j with
    | Json.Array (arr) ->
        let alen = Array.length arr in
          if alen < 1 then
            raise_short_array alen 1
          else if not (Json.is_string arr.(0)) then
            raise_unexpected_json_type (Json.string_of_type j) "string"
          else
            (to_string arr.(0)), arr
    | _ ->
        raise_unexpected_json_type (Json.string_of_type j) "array"

let to_array j =
  match j with
    | Json.Array arr -> arr
    | _ -> raise_unexpected_json_type (Json.string_of_type j) "array"

let array_elem arr i =
  check_array_with_length arr (i + 1);
  arr.(i)

let to_list j =
  Array.to_list (to_array j)

type object_table = (string * Json.t) list

let to_object_table j =
  match j with
    | Json.Object a -> Array.to_list a
    | _ -> raise_unexpected_json_type (Json.string_of_type j) "object"

let object_table_to_list j = j

let object_field t f =
  try List.assoc f t
  with Not_found -> raise_missing_object_field f

let optional_object_field t f =
  try List.assoc f t
  with Not_found -> Json.Null

let has_object_field t f =
  try ignore (List.assoc f t); true
  with Not_found -> false
