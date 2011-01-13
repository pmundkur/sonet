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
module U = Unix

type url = string
type payload = string
type meth = H.Request_header.meth

type request =
  | Payload of url list * payload option
  | FileRecv of url list * Unix.file_descr
  | FileSend of url list * Unix.file_descr

type error =
  | Unix of Unix.error
  | Other of string

exception Invalid_request of request
exception Invalid_url of url * string

type result = {
  meth : H.Request_header.meth;
  url : url;
  response : H.Response.t option;
  error : (url * error) list option;
}

type callback = {
  c_req : request;
  c_meth : H.Request_header.meth;
  c_fdofs : int;
  c_url : url;
  c_alternates : url list;
  c_results : result list ref;
  mutable c_response : H.Response.t option;
  mutable c_error : (url * error) list option;
}

type state = callback array

module Conn = Http_client_conn.Make(struct type t = callback end)

let unopt opt e =
  match opt with
    | None -> raise e
    | Some v -> v

let defopt def = function
  | None -> def
  | Some v -> v

let close_conn final cb t =
  if final then begin
    let res = {
      meth = cb.c_meth;
      url = cb.c_url;
      response = cb.c_response;
      error = cb.c_error
    } in
      cb.c_results :=  res :: !(cb.c_results)
  end;
  Conn.close t

let file_receiver fd cb t s o l f =
  try
    ignore (Unix.write fd s o l)
  with
    | Unix.Unix_error (e, _, _) ->
        cb.c_error <- Some ((cb.c_url, Unix e) :: defopt [] cb.c_error);
        close_conn true cb t

let file_sender fd buffer cb t () =
  try
    let nbytes = Unix.read fd buffer 0 (String.length buffer) in
      (nbytes = 0), buffer, 0, nbytes
  with
    | Unix.Unix_error (e, _, _) ->
        cb.c_error <- Some ((cb.c_url, Unix e) :: defopt [] cb.c_error);
        close_conn true cb t;
        true, buffer, 0, 0

let response_callback cb t resp =
  assert (cb.c_response = None);
  cb.c_response <- Some resp;
  close_conn true cb t

let shutdown_callback cb t =
  close_conn true cb t

let error_callback cb restarter t e =
  let err = (match e with
               | Conn.Error_eventloop (e, _, _) -> Unix e
               | Conn.Error_http (_, m) -> Other m) in
  let c_error = Some ((cb.c_url, err) :: defopt [] cb.c_error) in
  match cb with
    | { c_alternates = [] } ->
        cb.c_error <- c_error;
        close_conn true cb t
    | { c_alternates = c_url :: rest } ->
        let new_cb = { cb with c_url; c_alternates = rest; c_error } in
        let el = Conn.get_eventloop t in
          close_conn false cb t;
          restarter el new_cb

let scheme_host_port url =
  let uri = Uri.of_string url in
  let auth = unopt uri.Uri.authority (Invalid_url (url, "no host")) in
  let scheme = unopt uri.Uri.scheme (Invalid_url (url, "no scheme")) in
    scheme, auth.Uri.host, auth.Uri.port

let is_supported_url url =
  try ignore (scheme_host_port url); true
  with Invalid_url _ -> false

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

let reqhdr_of meth url hdrs =
  let u = Uri.of_string url in
    { H.Request_header.version = H.HTTP11;
      meth = meth;
      url = H.Request_header.Uri u;
      headers = (host_hdr u) @ hdrs }

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

let urls_of_req = function
  | Payload (urls, _) | FileRecv (urls, _) | FileSend (urls, _) -> urls

let fileofs_of_req =
  let ofs_of fd = Unix.lseek fd 0 Unix.SEEK_CUR in function
    | Payload (_, _) -> 0
    | FileRecv (_, fd) -> ofs_of fd
    | FileSend (_, fd) -> ofs_of fd

let reset_fileofs { c_req; c_fdofs } =
  match c_req with
    | Payload (_, _) -> ()
    | FileRecv (_, fd) -> assert (Unix.lseek fd c_fdofs Unix.SEEK_SET = c_fdofs)
    | FileSend (_, fd) -> assert (Unix.lseek fd c_fdofs Unix.SEEK_SET = c_fdofs)

let make_callback_arg c_meth c_req c_url c_alternates c_results =
  let c_fdofs = fileofs_of_req c_req in
    { c_req; c_meth; c_fdofs; c_url; c_alternates; c_results;
      c_response = None; c_error = None }

let content_length_hdr_of_fd fd =
  try [("Content-Length", [string_of_int (U.fstat fd).U.st_size])]
  with _ -> []

let content_length_hdr_of_payload = function
  | None -> [("Content-Length", ["0"])]
  | Some s -> [("Content-Length", [string_of_int (String.length s)])]

let make_file_recv_request meth url fd cb t =
  let prcb = file_receiver fd cb t in
    C.StreamingRecv ((req_of (reqhdr_of meth url []) None), prcb)

let make_file_send_request meth url fd cb t =
  let pscb = file_sender fd (String.create 8048) cb t in
  let cl_hdr = content_length_hdr_of_fd fd in
    C.StreamingSend ((reqhdr_of meth url cl_hdr), pscb)

let make_connect_callback meth cb_arg = function
  | Payload (_, payload_opt) ->
      let cl_hdr = content_length_hdr_of_payload payload_opt in
      let reqhdr = reqhdr_of meth cb_arg.c_url cl_hdr in
      let req = C.Small (req_of reqhdr payload_opt) in
        Conn.send_request req cb_arg
  | FileRecv (_, fd) ->
      Unix.ftruncate fd 0;
      (fun t ->
         let req = make_file_recv_request meth cb_arg.c_url fd cb_arg t in
           Conn.send_request req cb_arg t)
  | FileSend (_, fd) ->
      assert ((Unix.lseek fd 0 Unix.SEEK_SET) = 0);
      (fun t ->
         let req = make_file_send_request meth cb_arg.c_url fd cb_arg t in
           Conn.send_request req cb_arg t)

let rec make_conn el cb_funcs
    ({ c_url = url; c_alternates; c_error } as cb_arg) =
      let scheme, h, p = scheme_host_port url in
      let port = defopt 80 p in
      let err = (match addr_of_host h scheme with
                   | None ->
                       Some (Other ("Unable to resolve " ^ h))
                   | Some a ->
                       let addr = Unix.ADDR_INET (a, port) in
                       ignore (Conn.connect el addr cb_funcs);
                       None) in
        match err, c_alternates with
          | None, _ ->
              None
          | Some e, [] ->
              let error = Some ((url, e) :: defopt [] c_error) in
                Some { meth = cb_arg.c_meth; url; response = None; error }
          | Some e, c_url :: c_alternates ->
              let c_error = Some ((url, e) :: (defopt [] c_error)) in
              make_conn el cb_funcs { cb_arg with c_url; c_alternates; c_error }

let callbacks_with cb_arg connect_callback get_restarter =
  { Conn.connect_callback = connect_callback;
    response_callback = response_callback;
    shutdown_callback = shutdown_callback cb_arg;
    error_callback = error_callback cb_arg (get_restarter ()) }

let rec get_restarter () =
  (fun el cb_arg ->
     reset_fileofs cb_arg;
     start_conn el cb_arg)
and start_conn el cb_arg =
  let connect_cb = make_connect_callback cb_arg.c_meth cb_arg cb_arg.c_req in
  let cbs = callbacks_with cb_arg connect_cb get_restarter in
    match make_conn el cbs cb_arg with
      | None -> ()
      | Some res -> cb_arg.c_results := res :: !(cb_arg.c_results)

let start_requests el requests =
  let results = ref [] in
  let rec starter = function
    | [] ->
        ()
    | (meth, req) :: rest ->
        let url, alternates =
          (* We've ensured that there is at least one url *)
          match urls_of_req req with [] -> assert false | hd :: tl -> hd, tl in
        let cb_arg = make_callback_arg meth req url alternates results in
          start_conn el cb_arg;
          starter rest
  in
    starter requests;
    results

let request reqs =
  (* Sanity check the requests to ensure at least one url *)
  let () = List.iter (fun (_m, r) ->
                        if urls_of_req r = [] then raise (Invalid_request r)
                     ) reqs in
  let el = Eventloop.create () in
  let results_ref = start_requests el reqs in
    while Eventloop.has_connections el || Eventloop.has_timers el do
      Eventloop.dispatch el 1.0
    done;
    !results_ref
