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
  let logger s = if !verbose then Printf.printf "%s\n%!" s
  in Printf.ksprintf logger fmt

module ConnMap = Map.Make (struct type t = Unix.file_descr let compare = compare end)

(* A module that supports finding a timer by handle as well as by expiry time. *)
module Timers = struct

  type 'a entry = {
    handle : int;
    mutable expires_at : float;
    value : 'a;
  }

  module Timers_by_expiry = Map.Make (struct type t = float let compare = compare end)

  type 'a t = {
    mutable by_expiry : (('a entry) list) Timers_by_expiry.t;
  }

  let create () = { by_expiry = Timers_by_expiry.empty }

  let is_empty t = Timers_by_expiry.is_empty t.by_expiry

  let next_handle = ref 0

  let add_timer t at v =
    incr next_handle;
    let e = { handle = !next_handle; expires_at = at; value = v } in
    let es = (try Timers_by_expiry.find e.expires_at t.by_expiry
	      with Not_found -> [])
    in
      t.by_expiry <- Timers_by_expiry.add e.expires_at (e :: es) t.by_expiry;
      e

  let remove_timer t entry =
    let handle = entry.handle in
    let es = Timers_by_expiry.find entry.expires_at t.by_expiry in
    let es = List.filter (fun e' -> e'.handle <> handle) es in
      t.by_expiry <- (match es with
                        | [] -> Timers_by_expiry.remove entry.expires_at t.by_expiry
                        | _  -> Timers_by_expiry.add entry.expires_at es t.by_expiry
                     )

  exception Found of float

  (* Should only be called on a non-empty Timer set; otherwise,
     Not_found is raised. *)
  let get_first_expiry_time t =
    try
      (* This should give the earliest expiry time,
         since iteration is done in increasing order. *)
      Timers_by_expiry.iter (fun tim -> raise (Found tim)) t.by_expiry;
      raise Not_found
    with Found tim -> tim

  (* Extracts the timers for time t, and return a list of values for
     those timers *)
  let extract_timers_at t tim =
    try
      let es = Timers_by_expiry.find tim t.by_expiry in
        t.by_expiry <- Timers_by_expiry.remove tim t.by_expiry;
        List.map (fun e -> e.value) es
    with Not_found -> []

end

type error = Unix.error * string * string

type handle = Unix.file_descr

let handle_compare = compare
let handle_hash = Hashtbl.hash

type conn_status =
  | Connecting
  | Listening
  | Connected

type conn_callbacks = {
  accept_callback : t -> handle -> Unix.file_descr -> Unix.sockaddr -> unit;
  connect_callback : t -> handle -> unit;
  error_callback : t -> handle -> error -> unit;
  recv_ready_callback : t -> handle -> Unix.file_descr -> unit;
  send_ready_callback : t -> handle -> Unix.file_descr -> unit;
}

and conn_state = {
  mutable callbacks : conn_callbacks;
  mutable status : conn_status;
  mutable send_enabled : bool;
  mutable recv_enabled : bool;
}

and t = {
  mutable conns : conn_state ConnMap.t;
  mutable timers : (unit -> unit) Timers.t;
  poller : Net_events.poller;
  (* Unix.gettimeofday() at the time the loop iteration started *)
  mutable current_time : float;
  (* events currently being dispatched *)
  mutable cur_events : Net_events.event array;
  (* array index of event currently being dispatched *)
  mutable cur_ev_indx : int;
}

let create () = {
  conns = ConnMap.empty;
  timers = Timers.create ();
  poller = Unix_poller.create ();
  current_time = 0.0;
  cur_events = [||];
  cur_ev_indx = 0;
}

(* connections *)

let register_conn t fd ?(enable_send=false) ?(enable_recv=true) callbacks =
  let conn_state = { callbacks = callbacks;
                     status = Connected;
                     send_enabled = enable_send;
                     recv_enabled = enable_recv;
                   }
  in
    t.conns <- ConnMap.add fd conn_state t.conns;
    Unix.set_nonblock fd;
    t.poller.Net_events.add fd;
    if conn_state.recv_enabled then
      t.poller.Net_events.enable_recv fd;
    if conn_state.send_enabled then
      t.poller.Net_events.enable_send fd;
    fd

let remove_conn t handle =
  t.poller.Net_events.remove handle;
  Net_events.remove_events handle ~start_indx:t.cur_ev_indx t.cur_events;
  t.conns <- ConnMap.remove handle t.conns

let get_fd _t handle = handle

let connect t handle addr =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.status <- Connecting;
    try
      Unix.connect handle addr;
      conn_state.status <- Connected;
      conn_state.callbacks.connect_callback t handle
    with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
          t.poller.Net_events.enable_recv handle;
          t.poller.Net_events.enable_send handle;
      | Unix.Unix_error (ec, f, s) ->
          conn_state.callbacks.error_callback t handle (ec, f, s)

let listen t handle =
  let conn_state = ConnMap.find handle t.conns in
    Unix.listen handle 5;
    t.poller.Net_events.enable_recv handle;
    conn_state.recv_enabled <- true;
    conn_state.status <- Listening

let enable_send t handle =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.send_enabled <- true;
    if conn_state.status = Connected then
      t.poller.Net_events.enable_send handle

let disable_send t handle =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.send_enabled <- false;
    if conn_state.status = Connected then
      t.poller.Net_events.disable_send handle

let enable_recv t handle =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.recv_enabled <- true;
    if conn_state.status = Connected then
      t.poller.Net_events.enable_recv handle

let disable_recv t handle =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.recv_enabled <- false;
    if conn_state.status = Connected then
      t.poller.Net_events.disable_recv handle

let set_callbacks t handle callbacks =
  let conn_state = ConnMap.find handle t.conns in
    conn_state.callbacks <- callbacks

let has_connections t = not (ConnMap.is_empty t.conns)

(* timers *)

type timer = (unit -> unit) Timers.entry

let start_timer t time_offset_sec cb =
  let at = Unix.gettimeofday () +. time_offset_sec in
    Timers.add_timer t.timers at cb

let cancel_timer t timer =
  Timers.remove_timer t.timers timer

let timer_compare tim1 tim2 = compare tim1.Timers.handle tim2.Timers.handle
let timer_hash tim = tim.Timers.handle

let has_timers t = not (Timers.is_empty t.timers)

(* event dispatch *)

let dispatch_read t fd cs =
  match cs.status with
    | Connecting ->
        (match Unix.getsockopt_error fd with
           | None ->
               cs.status <- Connected;
               if not cs.recv_enabled then
                 t.poller.Net_events.disable_recv fd;
               if not cs.send_enabled then
                 t.poller.Net_events.disable_send fd;
               cs.callbacks.connect_callback t fd
           | Some err ->
               cs.callbacks.error_callback t fd (err, "connect", "")
        )
    | Listening ->
        (try
           let afd, aaddr = Unix.accept fd in
             cs.callbacks.accept_callback t fd afd aaddr
         with
           | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
           | Unix.Unix_error (Unix.ECONNABORTED, _, _)
           | Unix.Unix_error (Unix.EINTR, _, _)
             -> ()
           | Unix.Unix_error (ec, f, s) ->
               cs.callbacks.error_callback t fd (ec, f, s)
        )
    | Connected ->
        if cs.recv_enabled
        then cs.callbacks.recv_ready_callback t fd fd
        else t.poller.Net_events.disable_recv fd

let dispatch_write t fd cs =
  match cs.status with
    | Connecting ->
        (match Unix.getsockopt_error fd with
           | None ->
               cs.status <- Connected;
               if not cs.recv_enabled then
                 t.poller.Net_events.disable_recv fd;
               if not cs.send_enabled then
                 t.poller.Net_events.disable_send fd;
               cs.callbacks.connect_callback t fd
           | Some err ->
               cs.callbacks.error_callback t fd (err, "connect", "")
        )
    | Listening ->
        (* This should never happen, since listening sockets
           are not set for writing.  But, to avoid a busy
           select loop in case this socket keeps firing for
           writes, we disable the write watch.  *)
        t.poller.Net_events.disable_send fd
    | Connected ->
        if cs.send_enabled
        then cs.callbacks.send_ready_callback t fd fd
        else t.poller.Net_events.disable_send fd

let dispatch_timers t =
  let break = ref false in
    while ((not (Timers.is_empty t.timers)) && (not !break)) do
      let first_expired = Timers.get_first_expiry_time t.timers in
        if first_expired > t.current_time then
          break := true
        else begin
          let cbs = Timers.extract_timers_at t.timers first_expired in
            List.iter (fun cb -> cb ()) cbs
        end
    done

let dispatch t interval =
  t.current_time <- Unix.gettimeofday ();
  let interval =
    if Timers.is_empty t.timers then interval
    else
      (* the blocking interval for select is the smaller of the
         specified interval, and the interval before which the
         earliest timer expires.  *)
      let block_until = if interval > 0.0 then t.current_time +. interval else t.current_time in
      let first_expiry = Timers.get_first_expiry_time t.timers in
      let block_until = (if first_expiry < block_until then first_expiry else block_until) in
      let interval = block_until -. t.current_time in
        if interval < 0.0 then 0.0 else interval
  in
  let opt_events =
    try Some (t.poller.Net_events.get_events interval)
    with Unix.Unix_error (Unix.EINTR, _, _) -> None in
  let process_event idx ev =
    t.cur_ev_indx <- idx;
    let fd = ev.Net_events.event_fd in
    let opt_cs = (try Some (ConnMap.find fd t.conns)
                  with Not_found -> None)
    in
      match opt_cs, ev.Net_events.event_type with
        | None, _
        | _, Net_events.Removed ->
            (* this happens when the connection is removed by a
               previous callback *)
            ()
        | Some cs, Net_events.Readable ->
            if t.poller.Net_events.is_recv_enabled fd
            then dispatch_read t fd cs
        | Some cs, Net_events.Writeable ->
            if t.poller.Net_events.is_send_enabled fd
            then dispatch_write t fd cs
        | Some cs, Net_events.PendingError ->
            (* dispatch any enabled callback *)
            if t.poller.Net_events.is_recv_enabled fd
            then dispatch_read t fd cs
            else if t.poller.Net_events.is_send_enabled fd
            then dispatch_write t fd cs
        | Some cs, Net_events.Error (ec, f, s) ->
            cs.callbacks.error_callback t fd (ec, f, s)
  in
    (match opt_events with
       | Some events ->
           t.cur_events <- events;
           Array.iteri process_event events
       | None -> ()
    );
    dispatch_timers t
