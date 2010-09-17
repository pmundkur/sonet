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

let verbose = ref false

let dbg fmt =
  let logger s = if !verbose then Printf.printf "%s\n%!" s in
    Printf.ksprintf logger fmt

type t = {
  ev_loop : Eventloop.t;
  ev_handle : Eventloop.handle;
  ev_fd : Unix.file_descr;

  mutable callbacks : callbacks;
  mutable send_done_enabled : bool;

  send_buf : Buffer.t;
}

and callbacks = {
  connect_callback : t -> unit;
  recv_callback : t -> string -> (* offset *) int -> (* length *) int -> unit;
  send_done_callback : t -> unit;
  shutdown_callback : t -> unit;
  error_callback : t -> Eventloop.error -> unit;
}

let compare t1 t2 = compare t1.ev_handle t2.ev_handle
let hash t = Eventloop.handle_hash t.ev_handle

module Conns = Conn_map.Make(struct type conn = t end)

let accept_callback _el _h _fd _addr =
  failwith "Async_conn.accept_callback: invalid use"

let connect_callback _el h =
  let conn = Conns.get_conn h in
    conn.callbacks.connect_callback conn

let recv_ready_callback _el h fd =
  let conn = Conns.get_conn h in
  let buflen = 512 in
  let buf = String.create buflen in
    try
      let read_bytes = Unix.read fd buf 0 buflen in
        if read_bytes = 0 then
          conn.callbacks.shutdown_callback conn
        else begin
          dbg "<- %s" (String.sub buf 0 read_bytes);
          conn.callbacks.recv_callback conn buf 0 read_bytes
        end
    with
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          ()
      | Unix.Unix_error (ec, f, s) ->
          conn.callbacks.error_callback conn (ec, f, s)

let send_ready_callback _el h fd =
  let conn = Conns.get_conn h in
  let payload = Buffer.contents conn.send_buf in
  let payload_len = String.length payload in
    if payload_len > 0 then begin
      try
	(match Unix.write fd payload 0 payload_len with
	   | 0 -> ()
	   | sent ->
	       dbg "-> %s" (String.sub payload 0 sent);
	       Buffer.clear conn.send_buf;
	       Buffer.add_substring conn.send_buf payload sent (payload_len - sent)
	)
      with
	| Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
	| Unix.Unix_error (Unix.EAGAIN, _, _)
	| Unix.Unix_error (Unix.EINTR, _, _) ->
	    ()
	| Unix.Unix_error (ec, f, s) ->
	    conn.callbacks.error_callback conn (ec, f, s)
    end;
    (* We may need to invoke the send_done_callback, but we may
       have dispatched an error_callback above.  So we need to ensure
       the connection is still active.
    *)
    if Conns.has_conn h && Buffer.length conn.send_buf = 0 then begin
      Eventloop.disable_send conn.ev_loop conn.ev_handle;
      if conn.send_done_enabled then
        conn.callbacks.send_done_callback conn
    end

let error_callback _el h err =
  let conn = Conns.get_conn h in
    conn.callbacks.error_callback conn err

let conn_callbacks = {
  Eventloop.accept_callback = accept_callback;
  Eventloop.connect_callback = connect_callback;
  Eventloop.error_callback = error_callback;
  Eventloop.recv_ready_callback = recv_ready_callback;
  Eventloop.send_ready_callback = send_ready_callback;
}

let attach ev_loop fd  ?(enable_send_done=false) ?(enable_recv=true) callbacks =
  let ev_handle = Eventloop.register_conn ev_loop fd ~enable_send:false ~enable_recv conn_callbacks in
  let conn = { ev_loop = ev_loop;
               ev_handle = ev_handle;
               ev_fd = fd;
               callbacks = callbacks;
               send_done_enabled = enable_send_done;
               send_buf = Buffer.create 16;
             }
  in
    Conns.add_conn ev_handle conn;
    conn

let detach conn =
  Eventloop.remove_conn conn.ev_loop conn.ev_handle;
  Conns.remove_conn conn.ev_handle

let close conn =
  (* It might already be detached; ignore this case. *)
  (try detach conn with _ -> ());
  (try Unix.close conn.ev_fd with _ -> ())

let enable_send_done conn =
  conn.send_done_enabled <- true;
  Eventloop.enable_send conn.ev_loop conn.ev_handle

let disable_send_done conn =
  conn.send_done_enabled <- false

let enable_recv conn =
  Eventloop.enable_recv conn.ev_loop conn.ev_handle

let disable_recv conn =
  Eventloop.disable_recv conn.ev_loop conn.ev_handle

let connect conn addr =
  Eventloop.connect conn.ev_loop conn.ev_handle addr

let send conn s =
  Buffer.add_string conn.send_buf s;
  Eventloop.enable_send conn.ev_loop conn.ev_handle

let send_substring conn s off len =
  Buffer.add_substring conn.send_buf s off len;
  Eventloop.enable_send conn.ev_loop conn.ev_handle

let send_buffer conn b =
  Buffer.add_buffer conn.send_buf b;
  Eventloop.enable_send conn.ev_loop conn.ev_handle

let has_pending_send conn =
  Buffer.length conn.send_buf > 0

let set_callbacks conn callbacks =
  conn.callbacks <- callbacks

let get_handle conn = conn.ev_handle
let get_eventloop conn = conn.ev_loop
let get_fd conn = conn.ev_fd
let get_send_buf conn = conn.send_buf
