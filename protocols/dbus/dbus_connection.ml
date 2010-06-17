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

module T = Dbus_type
module M = Dbus_message
module A = Dbus_auth
module C = Async_conn
module P = Dbus_message_parse
module MM = Dbus_message_marshal

type error =
  | Connection_pending
  | Authentication_pending
  | Authentication_failed
  | Disconnected_send

exception Connection_error of error
let raise_error e =
  raise (Connection_error e)

type conn_state = {
  (* recv state *)
  parse_state : P.state;
}

type state =
  | Connecting
  | Authenticating of A.client_context
  | Connected of conn_state
  | Disconnected

type t = {
  conn : C.t;
  mutable callbacks : callbacks;
  mutable state : state;
  mutable server_address : string;
}

and callbacks = {
  authenticated_callback : t -> unit;
  msg_received_callback : t -> M.t -> unit;
  shutdown_callback : t -> unit;
  error_callback : t -> Eventloop.error -> unit;
  send_done_callback : t -> unit;
}

module Conns = Conn_map.Make(struct type conn = t end)

let init_conn_state = {
  parse_state = P.init_state ();
}

let connect_callback aconn =
  let conn = Conns.get_conn (C.get_handle aconn) in
  let sender = C.send aconn in
  let auth_context, auth_state = A.init_client_context A.External sender in
    assert (conn.state = Connecting);
    match auth_state with
      | A.Auth_in_progress ->
          conn.state <- Authenticating auth_context
      | A.Auth_failed ->
          raise_error Authentication_failed
      | A.Auth_succeeded _ ->
          conn.state <- Connected init_conn_state;
          conn.callbacks.authenticated_callback conn

let send_done_callback aconn =
  let conn = Conns.get_conn (C.get_handle aconn) in
    conn.callbacks.send_done_callback conn

let error_callback aconn err =
  let conn = Conns.get_conn (C.get_handle aconn) in
    conn.callbacks.error_callback conn err

let shutdown_callback aconn =
  let conn = Conns.get_conn (C.get_handle aconn) in
    conn.callbacks.shutdown_callback conn

let recv_callback aconn s ofs len =
  let conn = Conns.get_conn (C.get_handle aconn) in
    (* Since we invoke the msg_received_callback, we need to check for
       the disconnected state at the start of each recursion. *)
  let rec receiver s ofs len =
    match conn.state with
      | Connecting ->
          (* We should have transitioned out of this state on the
             connect_callback. *)
          assert false
      | Authenticating auth_ctxt ->
          let sender = C.send aconn in
            (match A.parse_input auth_ctxt sender s ofs len with
               | A.Auth_in_progress ->
                   ()
               | A.Auth_failed ->
                   raise_error Authentication_failed
               | A.Auth_succeeded (addr, consumed) ->
                   conn.server_address <- addr;
                   conn.state <- Connected init_conn_state;
                   conn.callbacks.authenticated_callback conn;
                   receiver s (ofs + consumed) (len - consumed)
            )
      | Connected cs ->
          (match P.parse_substring cs.parse_state s ofs len with
             | P.Parse_incomplete s ->
                 let cs = { parse_state = s } in
                   conn.state <- Connected cs
             | P.Parse_result (m, remaining) ->
                 let cs = { parse_state = P.init_state () } in
                   conn.state <- Connected cs;
                   conn.callbacks.msg_received_callback conn m;
                   receiver s (ofs + len - remaining) remaining
          )
      | Disconnected ->
          (* End the recursion. *)
          ()
  in
    receiver s ofs len

let send conn msg =
  match conn.state with
    | Connecting ->
        raise_error Connection_pending
    | Authenticating _ ->
        raise_error Authentication_pending
    | Connected _cstate ->
        let marshaled_size = MM.compute_marshaled_size msg in
        let buffer = String.make marshaled_size '\000' in
        let marshaled_bytes = (MM.marshal_message T.Little_endian buffer
                                 ~offset:0 ~length:marshaled_size
                                 msg) in
          assert (marshaled_size = marshaled_bytes);
          C.send conn.conn buffer
    | Disconnected ->
        raise_error Disconnected_send

let attach ev_loop fd ?(connected=false) callbacks =
  let acallbacks = {
    Async_conn.connect_callback   = connect_callback;
    Async_conn.recv_callback      = recv_callback;
    Async_conn.send_done_callback = send_done_callback;
    Async_conn.shutdown_callback  = shutdown_callback;
    Async_conn.error_callback     = error_callback;
  } in
  let aconn = Async_conn.attach ev_loop fd acallbacks in
  let dconn = {
    conn      = aconn;
    callbacks = callbacks;
    state     = Connecting;
    server_address = "";
  } in
    Conns.add_conn (Async_conn.get_handle aconn) dconn;
    if connected then connect_callback aconn;
    dconn

let detach conn =
  Async_conn.detach conn.conn;
  conn.state <- Disconnected

let close conn =
  Async_conn.close conn.conn;
  conn.state <- Disconnected
