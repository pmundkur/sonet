(*
 * Copyright (C) 2009      Prashanth Mundkur.
 * Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>
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

(* An extremely inefficient but purely native poller implementation. *)

type t = {
  mutable readers : Unix.file_descr list;
  mutable writers : Unix.file_descr list;
  mutable errors  : Unix.file_descr list;
}

let create_t () = {
  readers = [];
  writers = [];
  errors  = [];
}

let add t fd =
  t.errors <- fd :: t.errors

let remove t fd =
  let filter fd' = fd' <> fd
  in 
    t.readers <- List.filter filter t.readers;
    t.writers <- List.filter filter t.writers;
    t.errors  <- List.filter filter t.errors

let enable_recv t fd =
  t.readers <- if List.mem fd t.readers then t.readers else fd :: t.readers


let disable_recv t fd =
  let filter fd' = fd' <> fd in
    t.readers <- List.filter filter t.readers

let enable_send t fd =
  t.writers <- if List.mem fd t.writers then t.writers else fd :: t.writers

let disable_send t fd =
  let filter fd' = fd' <> fd in
    t.writers <- List.filter filter t.writers

let is_recv_enabled t fd =
  List.mem fd t.readers

let is_send_enabled t fd =
  List.mem fd t.writers

let get_events t timeout =
  let readers, writers, errors = Unix.select t.readers t.writers t.errors timeout in
  let events =
    (List.map (fun fd -> {
                 Net_events.event_type = Net_events.Readable;
                 Net_events.event_fd = fd;
               }) readers)
    @ (List.map (fun fd -> {
                   Net_events.event_type = Net_events.Writeable;
                   Net_events.event_fd = fd;
                 }) writers)
    @ (List.map (fun fd -> {
                   Net_events.event_type = Net_events.PendingError;
                   Net_events.event_fd = fd;
                 }) errors)
  in
    Array.of_list events

let create () =
  let t = create_t () in {
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
