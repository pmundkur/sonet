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

module type CallbackType = sig type t end

type payload_send_callback =
    unit -> bool * string * (* offset *) int * (* length *) int
type payload_recv_callback = Http.payload_callback

type request =
  | Small of Http.Request.t
  | StreamingSend of Http.Request_header.t * payload_send_callback
  | StreamingRecv of Http.Request.t * payload_recv_callback
  | Streaming of (Http.Request_header.t
                  * payload_send_callback
                  * payload_recv_callback)

module type Conn =
sig

  type t
  type callback

  (* if the connection has not yet been established, the request will
     be queued to be sent once the connection is established *)
  val send_request : request -> callback -> t -> unit

  type error =
    | Error_eventloop of Eventloop.error
    | Error_http of callback * string

  val string_of_error : error -> string

  type callbacks = {
    (* this will be called if no requests have been queued to be sent *)
    connect_callback : t -> unit;
    response_callback : callback -> t -> Http.Response.t -> unit;
    shutdown_callback : t -> unit;
    error_callback : t -> error -> unit;
  }

  val connect : Eventloop.t -> Unix.sockaddr -> callbacks -> t
  val detach : t -> unit
  val close : t -> unit

  val get_eventloop : t -> Eventloop.t
end

module Make (Callback : CallbackType) : Conn with type callback = Callback.t
