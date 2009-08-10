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

type t

val create : unit -> t

(* connections *)

type handle
type error = Unix.error * string * string

type conn_callbacks = {
  accept_callback : t -> handle -> Unix.file_descr -> Unix.sockaddr -> unit;
  connect_callback : t -> handle -> unit;
  error_callback : t -> handle -> error -> unit;
  recv_ready_callback : t -> handle -> Unix.file_descr -> unit;
  send_ready_callback : t -> handle -> Unix.file_descr -> unit;
}

(* this is to allow collections indexed by connection handles. *)
val handle_compare : handle -> handle -> int
val handle_hash : handle -> int

(* Connection Management *)

(* by default, notifications for incoming data are disabled, and enabled for all others. *)
val register_conn : t -> Unix.file_descr -> ?enable_send:bool -> ?enable_recv:bool -> conn_callbacks -> handle
val remove_conn : t -> handle -> unit
val get_fd : t -> handle -> Unix.file_descr

val connect : t -> handle -> Unix.sockaddr -> unit
val listen : t -> handle -> unit

val enable_send : t -> handle -> unit
val disable_send : t -> handle -> unit

val enable_recv : t -> handle -> unit
val disable_recv : t -> handle -> unit

val set_callbacks : t -> handle -> conn_callbacks -> unit

(* Timers *)

type timer

(* Starts a timer that will fire once only, and return a handle to
   this timer, so that it can be cancelled before it fires. The timer
   is automatically cancelled once it has fired.
*)
val start_timer : t -> float (* offset, secs *) -> (unit -> unit) -> timer

(* Allows cancelling a timer before it fires. *)
val cancel_timer : t -> timer -> unit

(* Utilities for storing timer handles in data structures. *)
val timer_compare: timer -> timer -> int
val timer_hash: timer -> int

(* Event Dispatch *)

(* dispatch t intvl will block at most for intvl seconds, and dispatch
   any retrieved events and expired timers.
*)
val dispatch : t -> float -> unit


(* Event loop management *)

val has_timers : t -> bool

val has_connections : t -> bool
