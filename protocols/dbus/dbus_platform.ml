(*
 * Copyright (C) 2009      Prashanth Mundkur.
 * Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type endian =
  | Little_endian
  | Big_endian

exception Unknown_endian

external real_get_host_endianness : unit -> endian = "stub_get_host_endianness"

let inited = ref false

let get_host_endianness () =
  if not !inited then begin
    Callback.register_exception "onet.unknown_endian_exception" Unknown_endian;
    inited := true
  end;
  real_get_host_endianness ()

external float_to_bytes : float -> char array = "stub_float_to_bytes"
external real_float_of_bytes : char array -> float = "stub_float_of_bytes"

let float_of_bytes ba =
  let len = Array.length ba in
    if len <> 8 then
      invalid_arg (Printf.sprintf "Invalid array of size %d (size 8 expected)" len);
    real_float_of_bytes ba
