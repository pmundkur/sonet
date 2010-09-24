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

(* Known TODOs:
   - handle 100 continue
*)

module H = Http

module type CallbackType = sig type t end

module type Conn =
sig

  type t
  type callback

  type payload_send_callback = unit -> bool * string * int * int
  type payload_recv_callback = H.payload_callback

  type request =
    | Small of H.Request.t
    | StreamingSend of H.Request_header.t * payload_send_callback
    | StreamingRecv of H.Request.t * payload_recv_callback
    | Streaming of (H.Request_header.t
                    * payload_send_callback
                    * payload_recv_callback)

  val send_request : t -> request -> callback -> unit

  type error =
    | Error_eventloop of Eventloop.error
    | Error_http of callback * string

  val string_of_error : error -> string

  type callbacks = {
    (* called only for StreamingRecv and Streaming requests *)
    response_header_callback : callback -> t -> Http.Response_header.t -> unit;
    (* called only for Small and StreamingSend requests *)
    response_callback : callback -> t -> Http.Response.t -> unit;

    connect_callback : t -> unit;
    shutdown_callback : t -> unit;
    error_callback :  t -> error -> unit;
  }

  val connect : Eventloop.t -> Unix.sockaddr -> callbacks -> t
  val detach : t -> unit
  val close : t -> unit
end

module C = Async_conn

module Make (Callback : CallbackType) = struct

  type callback = Callback.t

  type error =
    | Error_eventloop of Eventloop.error
    | Error_http of callback * string

  type send_state =
    | Send_start
    | Send_payload

  type recv_state =
    | Empty
    | Normal of H.Response.state * callback
    | Headers of H.Response_header.state * callback
    | Payload of H.Payload.state * callback

  type payload_send_callback = unit -> bool * string * int * int
  type payload_recv_callback = H.payload_callback

  type request =
    | Small of H.Request.t
    | StreamingSend of H.Request_header.t * payload_send_callback
    | StreamingRecv of H.Request.t * payload_recv_callback
    | Streaming of (H.Request_header.t
                    * payload_send_callback
                    * payload_recv_callback)

  type t = {
    conn : C.t;
    callbacks : callbacks;
    mutable closed : bool;

    (* requests waiting to be sent, in order *)
    mutable send_queue : (request * callback) Queue.t;
    mutable send_state : send_state;

    (* requests awaiting responses, in order *)
    mutable recv_queue : (request * callback) Queue.t;
    mutable recv_state : recv_state;
  }

  and callbacks = {
    response_header_callback : callback -> t -> H.Response_header.t -> unit;
    response_callback : callback -> t -> H.Response.t -> unit;

    connect_callback : t -> unit;
    shutdown_callback : t -> unit;
    error_callback : t -> error -> unit;
  }

  module Conns = Conn_map.Make(struct type conn = t end)

  let connect_callback conn =
    let t = Conns.get_conn (C.get_handle conn) in
      t.callbacks.connect_callback t

  let error_callback conn e =
    let t = Conns.get_conn (C.get_handle conn) in
      t.callbacks.error_callback t (Error_eventloop e)

  let send_request t req cb =
    Queue.add (req, cb) t.send_queue;
    C.enable_send_done t.conn

  let adjust_recv_state t =
    if not t.closed then
      if Queue.is_empty t.recv_queue then begin
        C.disable_recv t.conn;
        t.recv_state <- Empty;
      end else begin
        C.enable_recv t.conn;
        let top, cb = Queue.top t.recv_queue in
          match top with
            | Small _
            | StreamingSend _ ->
                t.recv_state <- Normal (H.Response.init_state (), cb)
            | StreamingRecv _
            | Streaming _ ->
                t.recv_state <- Headers (H.Response_header.init_state (), cb)
      end

  let prepare_for_next_response t =
    ignore (Queue.pop t.recv_queue);
    adjust_recv_state t

  let append_to_recv_queue t reqcb =
    Queue.add reqcb t.recv_queue;
    if t.recv_state = Empty then adjust_recv_state t

  let send_done_callback conn =
    let t = Conns.get_conn (C.get_handle conn) in
      match Queue.is_empty t.send_queue with
        | true ->
            assert (t.send_state = Send_start);
            C.disable_send_done t.conn
        | false ->
            let (top, cb) as topcb = Queue.top t.send_queue in
              match (t.send_state, top) with
                | Send_start, Small req
                | Send_start, StreamingRecv (req, _) ->
                    H.Request.serialize (C.get_send_buf t.conn) req;
                    C.enable_send_done t.conn;
                    append_to_recv_queue t (Queue.pop t.send_queue)
                | Send_start, StreamingSend (reqhdr, _)
                | Send_start, Streaming (reqhdr, _, _) ->
                    H.Request_header.serialize (C.get_send_buf t.conn) reqhdr;
                    C.enable_send_done t.conn;
                    t.send_state <- Send_payload;
                    (* We might get a response before we finish
                       sending the request payload, so put the request
                       on the recv_queue now. *)
                    append_to_recv_queue t topcb;
                | Send_payload, Small _
                | Send_payload, StreamingRecv _ ->
                    assert false
                | Send_payload, StreamingSend (_, scb)
                | Send_payload, Streaming (_, scb, _) ->
                    let fin, payload, off, len = scb () in
                      C.send_substring t.conn payload off len;
                      if fin then begin
                        ignore (Queue.pop t.send_queue);
                        t.send_state <- Send_start;
                      end

  let get_payload_callback t =
    match fst (Queue.top t.recv_queue) with
      | Small _
      | StreamingSend _ ->
          assert false
      | StreamingRecv (_, prcb)
      | Streaming (_, _, prcb) ->
          prcb

  type result =
    | Response of H.Response.t
    | Response_header of H.Response_header.t
    | Error of string

  let do_parse t s o l =
    match t.recv_state with
      | Empty ->
          (* We got data from the peer when we weren't expecting any.
             For now, we'll just log it and mark it as consumed.
             TODO: handle this better. *)
          Printf.printf "Unexpected early response: %s\n%!" (String.sub s o l);
          None, l
      | Normal (st, cb) ->
          (match H.Response.parse_substring st s o l with
             | H.Response.Parse_incomplete st ->
                 t.recv_state <- Normal (st, cb);
                 None, l
             | H.Response.Result (resp, consumed) ->
                 prepare_for_next_response t;
                 Some (cb, Response resp), consumed
             | H.Response.Error s ->
                 (* We don't really know how much was consumed, but
                    just lets say we consumed it all, since it's
                    pointless to continue parsing any further. *)
                 Some (cb, Error s), l)
      | Headers (st, cb) ->
          (match H.Response_header.parse_substring st s o l with
             | H.Response_header.Parse_incomplete st ->
                 t.recv_state <- Headers (st, cb);
                 None, l
             | H.Response_header.Result (resphdr, consumed) ->
                 (match (H.Payload.init_from_response
                           ~payload_callback:(get_payload_callback t)
                           resphdr) with
                    | H.Payload.No_payload ->
                        prepare_for_next_response t;
                        Some (cb, Response_header resphdr), consumed
                    | H.Payload.Payload st ->
                        t.recv_state <- Payload (st, cb);
                        Some (cb, Response_header resphdr), consumed
                    | H.Payload.Error s ->
                        prepare_for_next_response t;
                        Some (cb, Error s), consumed))
      | Payload (st, cb) ->
          (match H.Payload.parse_substring st s o l with
             | H.Payload.Parse_incomplete st ->
                 t.recv_state <- Payload (st, cb);
                 None, l
             | H.Payload.Result (_, consumed) ->
                 prepare_for_next_response t;
                 None, consumed)

  let recv_callback conn s o l =
    let t = Conns.get_conn (C.get_handle conn) in
    let last_offs = o + l in
    let rec dispatcher off =
      let len = last_offs - off in
        if len > 0 && not t.closed then begin
          try
            match do_parse t s off len with
              | None, consumed ->
                  dispatcher (off + consumed)
              | Some (cb, Response resp), consumed ->
                  t.callbacks.response_callback cb t resp;
                  dispatcher (off + consumed)
              | Some (cb, Response_header resphdr), consumed ->
                  t.callbacks.response_header_callback cb t resphdr;
                  dispatcher (off + consumed)
              | Some (cb, Error msg), consumed ->
                  t.callbacks.error_callback t (Error_http (cb, msg));
                  dispatcher (off + consumed)
          with
            | H.Headers.Http_error e ->
                let cb = snd (Queue.top t.recv_queue) in
                  t.callbacks.error_callback t
                    (Error_http (cb, (H.Headers.string_of_error e)))
            | H.Response_header.Http_error e ->
                let cb = snd (Queue.top t.recv_queue) in
                  t.callbacks.error_callback t
                    (Error_http (cb, (H.Response_header.string_of_error e)))
            | H.Response.Http_error e ->
                let cb = snd (Queue.top t.recv_queue) in
                t.callbacks.error_callback t
                  (Error_http (cb, (H.Response.string_of_error e)))
            | H.Payload.Http_error e ->
                let cb = snd (Queue.top t.recv_queue) in
                  t.callbacks.error_callback t
                    (Error_http (cb, (H.Payload.string_of_error e)))
        end
    in dispatcher o

  let shutdown_callback conn =
    let t = Conns.get_conn (C.get_handle conn) in
      match t.recv_state with
        | Empty ->
            (* The peer shutdown before we finished sending out any
               request. *)
            t.callbacks.shutdown_callback t
        | Normal (st, cb) ->
            H.Response.connection_closed st;
            (match H.Response.get_parse_result st with
               | None ->
                   t.callbacks.shutdown_callback t
               | Some resp ->
                   prepare_for_next_response t;
                   t.callbacks.response_callback cb t resp
            );
        | Headers _ ->
            t.callbacks.shutdown_callback t
        | Payload (st, _) ->
            H.Payload.connection_closed st;
            t.callbacks.shutdown_callback t

  let connect ev_loop addr callbacks =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let acallbacks = {
      C.connect_callback   = connect_callback;
      C.recv_callback      = recv_callback;
      C.send_done_callback = send_done_callback;
      C.shutdown_callback  = shutdown_callback;
      C.error_callback     = error_callback;
    } in
    let aconn = C.attach ev_loop sock acallbacks in
    let t = {
      conn       = aconn;
      callbacks  = callbacks;
      closed     = false;

      send_queue = Queue.create ();
      send_state = Send_start;

      recv_queue = Queue.create ();
      recv_state = Empty;
    } in
      Conns.add_conn (C.get_handle aconn) t;
      C.connect aconn addr;
      t

  let detach t =
    Conns.remove_conn (C.get_handle t.conn);
    C.detach t.conn

  let close t =
    t.closed <- true;
    (try detach t with _ -> ());
    C.close t.conn

  let string_of_error = function
    | Error_eventloop (e, f, s) ->
        Printf.sprintf "Eventloop: %s (%s): %s"
          f s (Unix.error_message e)
    | Error_http (_, msg) ->
        Printf.sprintf "Http: %s" msg
end
