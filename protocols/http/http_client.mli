(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010, 2011  Prashanth Mundkur.                          *)
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

type meth = Http.Request_header.meth
type url = string
type payload = string

type request =
  | Payload of url list * payload option
  | FileRecv of url list * Unix.file_descr
  | FileSend of url list * Unix.file_descr

type request_id = int

type error =
  | Unix of Unix.error
  | Http of (* status code *) int * string
  | Other of string

val string_of_error : error -> string

exception Invalid_request of request
exception Invalid_url of url * string

val is_supported_url : url -> bool

type result = {
  request_id : request_id;
  meth : Http.Request_header.meth;
  url : url;
  response : Http.Response.t option;
  error : (url * error) list option;
}

val request : ?retry_rounds:int -> (meth * request * request_id) list -> result list
