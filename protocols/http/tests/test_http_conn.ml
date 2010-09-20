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
module Client = Http_client_conn

let unopt = function
  | None -> assert false
  | Some v -> v

let addr_of_url url =
  let uri = Uri.of_string url in
    (unopt uri.Uri.scheme), (unopt (uri.Uri.authority)).Uri.host

let addr_of_host h scheme =
  let rec get_suitable = function
    | [] -> None
    | { Unix.ai_addr = Unix.ADDR_INET (a, _) } :: _ -> Some a
    | _ :: rest -> get_suitable rest
  in
    get_suitable (Unix.getaddrinfo h scheme
                    [ Unix.AI_FAMILY Unix.PF_INET;
                      Unix.AI_SOCKTYPE Unix.SOCK_STREAM])
let log_resphdr u hdr =
  Printf.printf "%s: %d %s\n%!"
    u hdr.H.Response_header.status_code hdr.H.Response_header.reason_phrase

let log_resp u resp ~show_payload =
  let hdr = resp.H.Response.response in
  let payload =
    match resp.H.Response.payload with
      | None -> ""
      | Some p -> Buffer.contents p.H.Payload.content
  in
  Printf.printf "%s: %d %s\n%s\n%!"
    u hdr.H.Response_header.status_code hdr.H.Response_header.reason_phrase
    (if show_payload then payload else "")

let make_small _t url =
  let host = snd (addr_of_url url) in
  let reqhdr = { H.Request_header.version = H.HTTP11;
                 meth = H.Request_header.Get;
                 url = H.Request_header.Uri (Uri.of_string "/");
                 headers = ["host", [host]]
               } in
    Client.Small { H.Request.request = reqhdr; payload = None }
let make_stream_recv t url =
  let host = snd (addr_of_url url) in
  let reqhdr = { H.Request_header.version = H.HTTP11;
                 meth = H.Request_header.Get;
                 url = H.Request_header.Uri (Uri.of_string "/");
                 headers = ["host", [host]]
               } in
  let cb t s o l f =
    Printf.eprintf "%s%!" (String.sub s o l);
    if f then Client.close t
  in Client.StreamingRecv ({ H.Request.request = reqhdr; payload = None },
                           cb t)

let get_url el u p =
  let cbs = {
    Client.connect_callback =
      (fun _ -> ());
    Client.response_header_callback =
      (fun _ _ rhdr -> log_resphdr u rhdr);
    Client.response_callback =
      (fun t _ resp ->
         log_resp u resp ~show_payload:false;
         Client.close t);
    Client.shutdown_callback =
      (fun t -> Client.close t);
    Client.error_callback =
      (fun t e ->
         Printf.eprintf "%s: %s\n%!" u (Client.string_of_error e));
  } in
  let scheme, host = addr_of_url u in
    match addr_of_host host scheme with
      | None ->
          Printf.eprintf "%s: Unable to resolve host %s\n%!" u host
      | Some a ->
          let t = Client.connect el (Unix.ADDR_INET (a, p)) cbs in
          let req = make_small t u in
          let sreq = make_stream_recv t u in
            ignore (Client.send_request t req);
            ignore (Client.send_request t sreq)

let run () =
  let el = Eventloop.create () in
    List.iter (fun (u, p) ->
                 get_url el u p
              ) [ "http://www.lwn.net", 80;
                  "http://www.nytimes.com", 80 ];
    while Eventloop.has_connections el || Eventloop.has_timers el do
      Eventloop.dispatch el 1.0
    done

let _ =
  Printexc.record_backtrace true;
  try run ()
  with e ->
    Printf.eprintf "%s\n%s\n%!"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
