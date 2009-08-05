(*
 * Copyright (C) 2009 Citrix Ltd.
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

let is_digit c =
  match c with '0' .. '9' -> true | _ -> false
let is_hex c =
  match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let digit_value c =
  (Char.code c) - (Char.code '0')
let hex_value c =
  match c with
    | '0' .. '9' -> digit_value c
    | 'a' .. 'f' -> 10 + (Char.code c) - (Char.code 'a')
    | 'A' .. 'F' -> 10 + (Char.code c) - (Char.code 'A')
    | _ -> 0 (* should never happen if guarded with is_hex *)
