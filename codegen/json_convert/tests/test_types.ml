(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type base_type =
  | B_int of int
  | B_int64 of int64
  | B_bool of bool
  | B_string of string
  | B_float of float

type simple_type =
  | S_int_option of int option
  | S_int64_option of int64 option
  | S_bool_option of bool option
  | S_string_option of string option
  | S_float_option of float option

  | S_int_list of int list
  | S_bool_list of bool list
  | S_int64_list of int64 list
  | S_string_list of string list

  | S_int_array of int array
  | S_bool_array of bool array
  | S_int64_array of int64 array
  | S_string_array of string array

(*** json-pragma:  set_record_prefix=record_  ***)

type record_type = {
  record_int: int;
  record_int64: int64;
  record_bool: bool;
  record_string: string;

  record_int_list: int list;
  record_int64_option_array: (int64 option) array;
  record_bool_array: bool array;

  record_prod_list: ((int * bool) list) * string;
}

(*** json-pragma:  clear_record_prefix  ***)

type complex_type1 = ((int list) * bool) array

type complex_type2 = {
  record: record_type;
  complex_type1: complex_type1;
}


(*** json-pragma: open Json_conv ***)
(*** json-pragma: use_converter int64 for Int64.t ***)

type t = Int64.t
