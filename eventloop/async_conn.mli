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

(* Convenience asynchronous connection, for use with the Eventloop
   module.

   This module provides a more convenient API for the IO callbacks.
*)

type t
val compare : t -> t -> int
val hash : t -> int

type callbacks = {
  connect_callback : t -> unit;
  recv_callback : t -> string -> (* offset *) int -> (* length *) int -> unit;
  send_done_callback : t -> unit;
  shutdown_callback : t -> unit;
  error_callback : t -> Eventloop.error -> unit;
}

val attach : Eventloop.t -> Unix.file_descr -> ?enable_send_done:bool -> ?enable_recv:bool -> callbacks -> t
val detach : t -> unit
val close  : t -> unit

val enable_send_done  : t -> unit
val disable_send_done : t -> unit

val enable_recv  : t -> unit
val disable_recv : t -> unit

val connect : t -> Unix.sockaddr -> unit

val send : t -> string -> unit
val send_substring : t -> string -> (* offset *) int -> (* length *) int -> unit
val send_buffer : t -> Buffer.t -> unit
val has_pending_send : t -> bool

val set_callbacks : t -> callbacks -> unit

val get_handle : t -> Eventloop.handle
val get_eventloop : t -> Eventloop.t
val get_fd : t -> Unix.file_descr
val get_send_buf : t -> Buffer.t
