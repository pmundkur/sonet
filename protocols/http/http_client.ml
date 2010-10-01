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

module H = Http
module C = Http_client_conn

type url = string
type payload = string
type meth = H.Request_header.meth

type request =
  | Payload of url * payload option
  | FileRecv of url * Unix.file_descr
  | FileSend of url * Unix.file_descr

type error =
  | Unix of Unix.error
  | Other of string

type result = {
  meth : H.Request_header.meth;
  url : string;
  response : H.Response.t option;
  error : error option;
}

type callback = {
  c_meth : H.Request_header.meth;
  c_url : string;
  mutable c_response : H.Response.t option;
  mutable c_error : error option;
  mutable c_results : result list ref;
}

type state = callback array

module Conn = Http_client_conn.Make(struct type t = callback end)

let unopt = function
  | None -> assert false
  | Some v -> v

let defopt def = function
  | None -> def
  | Some v -> v

let close_conn cb t =
  let res = {
    meth = cb.c_meth;
    url = cb.c_url;
    response = cb.c_response;
    error = cb.c_error
  } in
    cb.c_results :=  res :: !(cb.c_results);
    Conn.close t

let file_receiver fd cb t s o l f =
  try
    ignore (Unix.write fd s o l)
  with
    | Unix.Unix_error (e, _, _) ->
        cb.c_error <- Some (Unix e);
        close_conn cb t

let file_sender fd buffer cb t () =
  try
    let nbytes = Unix.read fd buffer 0 (String.length buffer) in
      (nbytes = 0), buffer, 0, nbytes
  with
    | Unix.Unix_error (e, _, _) ->
        cb.c_error <- Some (Unix e);
        close_conn cb t;
        true, buffer, 0, 0

let connect_callback req cb t =
  Conn.send_request t req cb

let response_callback cb t resp =
  assert (cb.c_response = None);
  cb.c_response <- Some resp;
  close_conn cb t

let shutdown_callback cb t =
  close_conn cb t

let error_callback cb t e =
  cb.c_error <- Some (match e with
                        | Conn.Error_eventloop (e, _, _) -> Unix e
                        | Conn.Error_http (_, m) -> Other m);
  close_conn cb t

let scheme_host_port url =
  let uri = Uri.of_string url in
  let auth = unopt uri.Uri.authority in
  let scheme = unopt uri.Uri.scheme in
    scheme, auth.Uri.host, auth.Uri.port

let addr_of_host h scheme =
  let rec get_suitable = function
    | [] -> None
    | { Unix.ai_addr = Unix.ADDR_INET (a, _) } :: _ -> Some a
    | _ :: rest -> get_suitable rest
  in
    get_suitable (Unix.getaddrinfo h scheme
                    [ Unix.AI_FAMILY Unix.PF_INET;
                      Unix.AI_SOCKTYPE Unix.SOCK_STREAM ])

let host_hdr uri =
  match uri.Uri.authority with
    | None -> []
    | Some a -> [ ("Host", [a.Uri.host]) ]

let reqhdr_of meth url =
  let u = Uri.of_string url in
    { H.Request_header.version = H.HTTP11;
      meth = meth;
      url = H.Request_header.Uri u;
      headers = host_hdr u }

let payload_of = function
  | None -> None
  | Some s ->
      let buf = Buffer.create (String.length s) in
        Buffer.add_string buf s;
        Some { H.Payload.content = buf;
               trailers = [] }

let req_of reqhdr payload_opt =
  { H.Request.request = reqhdr;
    payload = payload_of payload_opt }

let make_callback meth url results =
  { c_meth = meth; c_url = url; c_response = None; c_error = None; c_results = results }

let callbacks_with cb connect_callback =
  { Conn.connect_callback = connect_callback;
    response_callback = response_callback;
    shutdown_callback = shutdown_callback cb;
    error_callback = error_callback cb }

let make_file_recv_request meth url fd cb t =
  let prcb = file_receiver fd cb t in
  C.StreamingRecv ((req_of (reqhdr_of meth url) None), prcb)

let make_file_send_request meth url fd cb t =
  let pscb = file_sender fd (String.create 1024) cb t in
    C.StreamingSend ((reqhdr_of meth url), pscb)

let make_callbacks results meth = function
  | Payload (url, payload_opt) ->
      let req = C.Small (req_of (reqhdr_of meth url) payload_opt) in
      let cb = make_callback meth url results in
        callbacks_with cb (Conn.send_request req cb)
  | FileRecv (url, fd) ->
      let cb = make_callback meth url results in
      let connect_callback =
        (fun t ->
           Conn.send_request (make_file_recv_request meth url fd cb t) cb t) in
        callbacks_with cb connect_callback
  | FileSend (url, fd) ->
      let cb = make_callback meth url results in
      let connect_callback =
        (fun t ->
           Conn.send_request (make_file_send_request meth url fd cb t) cb t) in
        callbacks_with cb connect_callback

let get_url = function
  | Payload (url, _) | FileRecv (url, _) | FileSend (url, _) -> url

let make_conn el results meth req =
  let url = get_url req in
  let scheme, h, p = scheme_host_port url in
  let port = defopt 80 p in
    match addr_of_host h scheme with
      | None ->
          Some ("Unable to resolve " ^ h)
      | Some a ->
          let cbs = make_callbacks results meth req in
            ignore (Conn.connect el (Unix.ADDR_INET (a, port)) cbs);
            None

let start_requests el requests =
  let results = ref [] in
  let rec starter = function
    | [] ->
        ()
    | (meth, req) :: rest ->
        (match make_conn el results meth req with
           | None ->
               ()
           | Some errmsg ->
               let res = { meth = meth;
                           url = (get_url req);
                           response = None;
                           error = Some (Other errmsg) }
               in results :=  res :: !results);
        starter rest
  in
    starter requests;
    results

let make_requests reqs =
  let el = Eventloop.create () in
  let results_ref = start_requests el reqs in
    while Eventloop.has_connections el || Eventloop.has_timers el do
      Eventloop.dispatch el 1.0
    done;
    !results_ref
