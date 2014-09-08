(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-2012  Prashanth Mundkur.                           *)
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

type error =
  | Unix of Unix.error
  | Http of (* status code *) int * string
  | Inactive_connection
  | Other of string

exception Invalid_request of string
exception Invalid_url of url * string

type request =
  | Payload of url list * payload option
  | FileRecv of url list * Unix.file_descr
  | FileSend of url list * Unix.file_descr

type response =
  | Success of H.Response.t * (url * error) list
  | Failure of (url * error) * (url * error) list

module type C = sig
  type request_id

  type result = {
    request_id : request_id;
    meth : H.meth;
    url : url;
    response : response;
  }

  val request : (?retry_rounds:int -> ?timeout:float
    -> (H.meth * request * request_id) list -> result list)
end

module type RequestId = sig type t end

(* utilities *)

let string_of_error = function
  | Unix e -> Unix.error_message e
  | Http (sc, m) -> Printf.sprintf "HTTP status %d: %s" sc m
  | Inactive_connection -> Printf.sprintf "Inactive connection"
  | Other m -> m

let unopt opt e =
  match opt with
  | None -> raise e
  | Some v -> v

let defopt def = function
  | None -> def
  | Some v -> v

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

(* implementation *)

module Make (Id : RequestId) = struct
  type request_id = Id.t

  type result = {
    request_id : request_id;
    meth : H.meth;
    url : url;
    response : response;
  }

  type callback = {
    c_req : request;
    c_req_id : request_id;
    c_meth : H.meth;
    c_fdofs : int;
    c_url : url;
    c_alternates : url list;
    c_retries : int;
    c_activity : bool ref;
    c_timer : Eventloop.timer option ref;
    c_results : result list ref;
    c_response : H.Response.t option;
    c_error : (url * error) list option;
  }

  module Conn = C.Make (struct type t = callback end)

  let make_response cba =
    match cba.c_response, cba.c_error with
    | None, Some []
    | None, None -> assert false
    | Some r, None -> Success (r, [])
    | Some r, Some e -> Success (r, e)
    | None, Some (e :: er) -> Failure (e, er)

  let close_conn final cba t =
    if final then begin
      let res = {
        request_id = cba.c_req_id;
        meth = cba.c_meth;
        url = cba.c_url;
        response = make_response cba
      } in
      cba.c_results :=  res :: !(cba.c_results)
    end;
    (match !(cba.c_timer) with
     | Some th ->
         Eventloop.cancel_timer (Conn.get_eventloop t) th;
         cba.c_timer := None
     | None ->
         ()
    );
  Conn.close t

  let update_error cba err =
    { cba with c_error = Some ((cba.c_url, err) :: defopt [] cba.c_error) }

  let file_receiver fd cba t s o l f =
    try
      ignore (Unix.write fd s o l)
    with
    | Unix.Unix_error (e, _, _) ->
        let cba = update_error cba (Unix e) in
        close_conn true cba t

  let file_sender fd buffer cba t () =
    try
      let nbytes = Unix.read fd buffer 0 (String.length buffer) in
      (nbytes = 0), buffer, 0, nbytes
    with Unix.Unix_error (e, _, _) ->
      let cba = update_error cba (Unix e) in
      close_conn true cba t;
      true, buffer, 0, 0

  let restart_after_error t e cba restarter =
    let c_retries = cba.c_retries - 1 in
    let do_restart cba =
      let el = Conn.get_eventloop t in
      close_conn false cba t;
      restarter el cba in
    match cba with
    | { c_retries } when c_retries <= 0 ->
        close_conn true (update_error cba e) t
    | { c_alternates = [] } ->
        (* Retry the same url. *)
        do_restart { cba with c_retries }
    | { c_url; c_alternates = h :: t } ->
        (* Retry with the next alternative. *)
        do_restart { cba with c_url = h; c_alternates = t @ [ c_url ]; c_retries }

  let response_callback restarter cba t resp =
    assert (cba.c_response = None);
    let status = H.Response.status_code resp in
    if status >= 200 && status < 299 then begin
      close_conn true { cba with c_response = Some resp } t
    end else if status = 503 then begin
      (* Server is overloaded; try an alternative. If there is only
         one alternative, it would be nicer to use a timer. *)
      close_conn false cba t;
      restart_after_error t (Http (status, "Server busy")) cba restarter
    end else if status = 300 || status = 301 || status = 302 then begin
      (* Perform redirection.  303 is not currently handled, and will
         be treated like an error. *)
      let headers = H.Response.headers resp in
      try
        match H.lookup_header "Location" headers with
        | [] ->
            restart_after_error t (Http (status, "HTTP redirect with empty location"))
              cba restarter
        | l :: _ ->
            (* Redirect without logging an error *)
            let new_cba = { cba with c_url = l } in
            let el = Conn.get_eventloop t in
            close_conn false cba t;
            restarter el new_cba
      with Not_found ->
        restart_after_error t (Http (status, "HTTP redirect with no location"))
          cba restarter
    end else
      restart_after_error t (Http (status, "Unexpected response")) cba restarter

  let shutdown_callback restarter cba t =
    (* If we'd received a complete response, we would have received a
       response_callback by now, so getting this callback is equivalent
       to an error callback.  *)
    assert (cba.c_response = None);
    restart_after_error t (Other "connection shutdown") cba restarter

  let error_callback restarter cba t e =
    let err =
      match e with
      | Conn.Error_eventloop (e, _, _) -> Unix e
      | Conn.Error_http (_, m) -> Other m
    in restart_after_error t err cba restarter

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
    { c_req; c_req_id; c_meth; c_fdofs; c_url; c_alternates;
      c_retries; c_activity = ref false; c_timer = ref None;
      c_results; c_response = None; c_error = None }

  let content_length_hdr_of_fd fd =
    try [("Content-Length", [string_of_int (Unix.fstat fd).Unix.st_size])]
    with _ -> []

  let content_length_hdr_of_payload = function
    | None -> [("Content-Length", ["0"])]
    | Some s -> [("Content-Length", [string_of_int (String.length s)])]

  let make_file_recv_request meth url fd cba t =
    let prcb = file_receiver fd cba t in
    C.StreamingRecv ((req_of (reqhdr_of meth url []) None), prcb)

  let make_file_send_request meth url fd cba t =
    let pscb = file_sender fd (String.create 8048) cba t in
    let cl_hdr = content_length_hdr_of_fd fd in
    C.StreamingSend ((reqhdr_of meth url cl_hdr), pscb)

  let make_connect_callback meth cba = function
    | Payload (_, payload_opt) ->
        let cl_hdr = content_length_hdr_of_payload payload_opt in
        let reqhdr = reqhdr_of meth cba.c_url cl_hdr in
        let req = C.Small (req_of reqhdr payload_opt) in
        Conn.send_request req cba
    | FileRecv (_, fd) ->
        (fun t ->
          let req = make_file_recv_request meth cba.c_url fd cba t in
          Conn.send_request req cba t)
    | FileSend (_, fd) ->
        (fun t ->
          let req = make_file_send_request meth cba.c_url fd cba t in
          Conn.send_request req cba t)

  let rec start_conn_timer timeout restarter el t cba =
    assert (!(cba.c_timer) = None);
    cba.c_activity := false;
    let timer () =
      cba.c_timer := None;
      if !(cba.c_activity) then start_conn_timer timeout restarter el t cba
      else restart_after_error t Inactive_connection cba restarter
    in
    cba.c_timer := Some (Eventloop.start_timer el timeout timer)

  let make_conn timeout el restarter cb_funcs
      ({ c_url; c_alternates; c_error; c_retries } as cba) =
    let scheme, h, p = scheme_host_port c_url in
    let port = defopt 80 p in
    let err =
      match addr_of_host h scheme with
      | None ->
          Some (Other ("Unable to resolve " ^ h))
      | Some a ->
          let addr = Unix.ADDR_INET (a, port) in
          let t = Conn.connect el addr cb_funcs in
          start_conn_timer timeout restarter el t cba;
          None in
    match err with
    | None -> `Started
    | Some e ->
        (if c_retries <= 0 then
          let response = Failure ((c_url, e), defopt [] c_error) in
          `Finished { request_id = cba.c_req_id; meth = cba.c_meth; url = c_url; response }
        else
          let cba = update_error cba e in
          let c_retries = c_retries - 1 in
          (match c_alternates with
           | [] ->
               (* Retry the same url. *)
               `Retry { cba with c_retries }
           | h :: t ->
               (* Retry with the next alternative. *)
               `Retry { cba with c_retries; c_url = h; c_alternates = t @ [ c_url ]}
          ))

  let callbacks_with cba connect_callback restarter =
    let activity_wrapper f x =
      cba.c_activity := true;
      f x in
    { Conn.connect_callback = activity_wrapper connect_callback;
      response_callback = activity_wrapper (response_callback restarter);
      shutdown_callback = activity_wrapper (shutdown_callback restarter cba);
      error_callback = activity_wrapper (error_callback restarter cba) }

  let rec get_restarter timeout =
    (fun el cba ->
      reset_fileofs cba;
      start_conn timeout el cba)
  and start_conn timeout el cba =
    let connect_cb = make_connect_callback cba.c_meth cba cba.c_req in
    let cbs = callbacks_with cba connect_cb (get_restarter timeout) in
    match make_conn timeout el (get_restarter timeout) cbs cba with
    | `Started         -> ()
    | `Retry retry_arg -> start_conn timeout el retry_arg
    | `Finished res    -> cba.c_results := res :: !(cba.c_results)

  let start_requests timeout retry_rounds el requests =
    let results = ref [] in
    let rec starter = function
      | [] ->
          ()
      | (meth, req, req_id) :: rest ->
          let url, alternates =
            (* We've ensured that there is at least one url *)
            match urls_of_req req with [] -> assert false | hd :: tl -> hd, tl in
          let retries = retry_rounds * (1 + List.length alternates) in
          let cba = make_callback_arg meth req req_id url alternates retries results in
          start_conn timeout el cba;
          starter rest
    in
    starter requests;
    results

  let dEFAULT_RETRY_ROUNDS = 2
  let dEFAULT_INACTIVITY_TIMEOUT = 30.0 (* seconds *)

  let request ?retry_rounds ?timeout reqs =
    let time_out = defopt dEFAULT_INACTIVITY_TIMEOUT timeout in
    let retries = defopt dEFAULT_RETRY_ROUNDS retry_rounds in
    (* Sanity check the requests to ensure at least one url *)
    let () = List.iter
        (fun (_m, r, _ri) ->
          if urls_of_req r = [] then raise (Invalid_request "no urls in request")
        ) reqs in
    let el = Eventloop.create () in
    let results_ref = start_requests time_out retries el reqs in
    while Eventloop.has_connections el || Eventloop.has_timers el do
      Eventloop.dispatch el 10.0
    done;
    !results_ref
end
