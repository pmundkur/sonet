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

type request_id = int

type error =
  | Unix of Unix.error
  | Http of (* status code *) int * string
  | Other of string

let string_of_error = function
  | Unix e -> Unix.error_message e
  | Http (sc, m) -> Printf.sprintf "HTTP status %d: %s" sc m
  | Other m -> m

exception Invalid_request of request
exception Invalid_url of url * string

type result = {
  request_id : request_id;
  meth : H.Request_header.meth;
  url : url;
  response : H.Response.t option;
  error : (url * error) list option;
}

type callback = {
  c_req : request;
  c_req_id : int;
  c_meth : H.Request_header.meth;
  c_fdofs : int;
  c_url : url;
  c_alternates : url list;
  c_retries : int;
  c_results : result list ref;
  mutable c_response : H.Response.t option;
  mutable c_error : (url * error) list option;
}

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
      request_id = cb.c_req_id;
      meth = cb.c_meth;
      url = cb.c_url;
      response = cb.c_response;
      error = cb.c_error;
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

let restart_after_error t e cb restarter =
  let c_retries = cb.c_retries - 1 in
  let c_error = Some ((cb.c_url, e) :: defopt [] cb.c_error) in
  let do_restart cb =
    let el = Conn.get_eventloop t in
      close_conn false cb t;
      restarter el cb
  in
    match cb with
      | { c_retries } when c_retries <= 0 ->
          cb.c_error <- c_error;
          close_conn true cb t
      | { c_alternates = [] } ->
          (* Retry the same url. *)
          do_restart { cb with c_retries }
      | { c_url; c_alternates = h :: t } ->
          (* Retry with the next alternative. *)
          do_restart { cb with c_url = h; c_alternates = t @ [ c_url ]; c_retries }


let response_callback restarter cb t resp =
  assert (cb.c_response = None);
  let status = resp.Http.Response.response.Http.Response_header.status_code in
    if status >= 200 && status < 299 then begin
      cb.c_response <- Some resp;
      close_conn true cb t
    end else if status = 503 then begin
      (* Server is overloaded; try an alternative. If there is only
         one alternative, it would be nicer to use a timer. *)
      close_conn false cb t;
      restart_after_error t (Http (status, "Server busy")) cb restarter
    end else if status = 300 or status = 301 or status = 302 then begin
      (* Perform redirection.  303 is not currently handled, and will
         be treated like an error. *)
      let headers = resp.Http.Response.response.Http.Response_header.headers in
        try
          match Http.lookup_header "Location" headers with
            | [] ->
                restart_after_error t (Http (status, "HTTP redirect with empty location"))
                  cb restarter
            | l :: _ ->
                (* Redirect without logging an error *)
                let new_cb = { cb with c_url = l } in
                let el = Conn.get_eventloop t in
                  close_conn false cb t;
                  restarter el new_cb
        with Not_found ->
          restart_after_error t (Http (status, "HTTP redirect with no location"))
            cb restarter
    end else
      restart_after_error t (Http (status, "Unexpected response")) cb restarter

let shutdown_callback restarter cb t =
  (* If we'd received a complete response, we would have received a
     response_callback by now, so getting this callback is equivalent
     to an error callback.  *)
  assert (cb.c_response = None);
  restart_after_error t (Other "connection shutdown") cb restarter

let error_callback restarter cb t e =
  let err = (match e with
               | Conn.Error_eventloop (e, _, _) -> Unix e
               | Conn.Error_http (_, m) -> Other m) in
    restart_after_error t err cb restarter

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
    get_suitable (Unix.getaddrinfo h ""
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
    | FileRecv (_, fd) -> Unix.ftruncate fd c_fdofs
    | FileSend (_, fd) -> assert (Unix.lseek fd c_fdofs Unix.SEEK_SET = c_fdofs)

let make_callback_arg c_meth c_req c_req_id c_url c_alternates c_retries c_results =
  let c_fdofs = fileofs_of_req c_req in
    { c_req; c_req_id; c_meth; c_fdofs; c_url; c_alternates; c_retries; c_results;
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
      (fun t ->
         let req = make_file_recv_request meth cb_arg.c_url fd cb_arg t in
           Conn.send_request req cb_arg t)
  | FileSend (_, fd) ->
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
            Some { request_id = cb_arg.c_req_id; meth = cb_arg.c_meth; url; response = None; error }
      | Some e, c_url :: c_alternates ->
          let c_error = Some ((url, e) :: (defopt [] c_error)) in
            make_conn el cb_funcs { cb_arg with c_url; c_alternates; c_error }

let callbacks_with cb_arg connect_callback get_restarter =
  let restarter = get_restarter () in
    { Conn.connect_callback = connect_callback;
      response_callback = response_callback restarter;
      shutdown_callback = shutdown_callback restarter cb_arg;
      error_callback = error_callback restarter cb_arg }

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

let start_requests el retry_rounds requests =
  let results = ref [] in
  let rec starter = function
    | [] ->
        ()
    | (meth, req, req_id) :: rest ->
        let url, alternates =
          (* We've ensured that there is at least one url *)
          match urls_of_req req with [] -> assert false | hd :: tl -> hd, tl in
        let retries = retry_rounds * (1 + List.length alternates) in
        let cb_arg = make_callback_arg meth req req_id url alternates retries results in
          start_conn el cb_arg;
          starter rest
  in
    starter requests;
    results

let dEFAULT_RETRY_ROUNDS = 2

let request ?retry_rounds reqs =
  (* Sanity check the requests to ensure at least one url *)
  let () = List.iter (fun (_m, r, _ri) ->
                        if urls_of_req r = [] then raise (Invalid_request r)
                     ) reqs in
  let el = Eventloop.create () in
  let results_ref = start_requests el (defopt dEFAULT_RETRY_ROUNDS retry_rounds) reqs in
    while Eventloop.has_connections el || Eventloop.has_timers el do
      Eventloop.dispatch el 1.0
    done;
    !results_ref
