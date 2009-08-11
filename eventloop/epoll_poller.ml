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

(* A poller implementation based on Linux epoll. *)

type t

external create_t : int -> t = "stub_epoll_create"

external add : t -> Unix.file_descr -> unit = "stub_epoll_add"

external remove : t -> Unix.file_descr -> unit = "stub_epoll_remove"

external enable_recv : t -> Unix.file_descr -> unit = "stub_epoll_enable_recv"

external disable_recv : t -> Unix.file_descr -> unit = "stub_epoll_disable_recv"

external enable_send : t -> Unix.file_descr -> unit = "stub_epoll_enable_send"

external disable_send : t -> Unix.file_descr -> unit = "stub_epoll_disable_send"

external is_recv_enabled : t -> Unix.file_descr -> bool = "stub_epoll_is_recv_enabled"

external is_send_enabled : t -> Unix.file_descr -> bool = "stub_epoll_is_send_enabled"

external get_events : t -> float -> Net_events.event array = "stub_epoll_get_events"

let create ?(size=1024) () =
  let t = create_t size in {
      Net_events.add    = add t;
      Net_events.remove = remove t;
      Net_events.enable_recv  = enable_recv t;
      Net_events.disable_recv = disable_recv t;
      Net_events.enable_send  = enable_send t;
      Net_events.disable_send = disable_send t;
      Net_events.is_recv_enabled = is_recv_enabled t;
      Net_events.is_send_enabled = is_send_enabled t;
      Net_events.get_events = get_events t;
    }
