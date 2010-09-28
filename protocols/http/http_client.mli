(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010      Prashanth Mundkur.                            *)
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
  | Payload of url * payload option
  | FileRecv of url * Unix.file_descr
  | FileSend of url * Unix.file_descr

type error =
  | Unix of Unix.error
  | Other of string

type result = {
  meth : Http.Request_header.meth;
  url : string;
  response : Http.Response.t option;
  error : error option;
}

val make_requests : (meth * request) list -> result list
