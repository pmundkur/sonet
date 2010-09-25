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

type authority = {
  userinfo : string option;
  host : string;
  port : int option;
}

type scheme = string

type t = {
  scheme : scheme option;
  authority : authority option;
  path : string option;
  query : string option;
  fragment : string option;
}

type error =
  | Invalid_uri of string
  | Invalid_authority of string
  | Invalid_port of string
  | Missing_required_components of string
exception Uri_error of error
val string_of_error : error -> string

val of_string : string -> t
val to_string : t -> string

val abspath_to_string : t -> string
val authority_to_string : t -> string

val normalize : t -> t
