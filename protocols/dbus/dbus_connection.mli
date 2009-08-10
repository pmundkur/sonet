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

type error =
  | Connection_pending
  | Authentication_pending
  | Authentication_failed
  | Disconnected_send
exception Connection_error of error

type t

type callbacks = {
  authenticated_callback : t -> unit;
  msg_received_callback : t -> Dbus_message.t -> unit;
  shutdown_callback : t -> unit;
  error_callback : t -> Eventloop.error -> unit;
  send_done_callback : t -> unit;
}

val send : t -> Dbus_message.t -> unit

val attach : Eventloop.t -> Unix.file_descr -> ?connected:bool -> callbacks -> t
val detach : t -> unit
val close : t -> unit
