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

module type ConnectionInfo =
sig
  type conn
end

module Make (ConnInfo : ConnectionInfo) =
struct

  module ConnectionMap = Map.Make (struct type t = Eventloop.handle
                                          let compare = Eventloop.handle_compare
                                   end)

  let conns = ref (ConnectionMap.empty : ConnInfo.conn ConnectionMap.t)

  (* Connection handling utilities *)

  let add_conn h conn =
    conns := ConnectionMap.add h conn !conns

  let get_conn h =
    ConnectionMap.find h !conns

  let has_conn h =
    ConnectionMap.mem h !conns

  let remove_conn h =
    conns := ConnectionMap.remove h !conns
end
