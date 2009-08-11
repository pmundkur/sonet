(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2009      Prashanth Mundkur.                            *)
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

type error =
  | Internal_error of string
  | Unexpected_char of char
  | Unrecognized_server_protocol of string

exception Auth_error of error

let raise_error e =
  raise (Auth_error e)

(* Authentication mechanisms *)

type mech_output =
  | Mech_continue of string
  | Mech_ok of string
  | Mech_error

class type mechanism = object
  method init : mech_output
  method challenge : string -> mech_output
end

class external_mech =
  let euid = Printf.sprintf "%d" (Unix.geteuid ()) in
  let euid = Utils.string_to_hex euid in
object
  val mutable response_sent = false
  method init =
    response_sent <- true; Mech_ok euid
  method challenge (data : string) =
    if response_sent then Mech_error
    else (response_sent <- true;
          Mech_continue euid)
end

class anonymous_mech =
object
  val mutable response_sent = false
  method init =
    response_sent <- true; Mech_ok "odbus"
  method challenge (data : string) =
    if response_sent then Mech_error
    else (response_sent <- true;
          Mech_continue "odbus")
end

type auth_mechanism =
  | External
  | Anonymous

(* Client and server protocols *)

type client_proto =
  | Client_auth of string * string option
  | Client_cancel
  | Client_begin
  | Client_data of string
  | Client_error of string

let client_proto_to_string = function
  | Client_auth (mech, None) -> Printf.sprintf "AUTH %s" mech
  | Client_auth (mech, Some init_resp) -> Printf.sprintf "AUTH %s %s" mech init_resp
  | Client_cancel -> "CANCEL"
  | Client_begin -> "BEGIN"
  | Client_data data -> Printf.sprintf "DATA %s" data
  | Client_error err -> Printf.sprintf "ERROR %s" err

type server_proto =
  | Server_rejected of string  (* TODO: make this a list of options *)
  | Server_ok of string
  | Server_data of string
  | Server_error
  | Server_other of string

let server_proto_of_string s =
  let slen = String.length s in
  let try_match_cmd cmd =
    let clen = String.length cmd in
      if slen >= clen && String.sub s 0 clen = cmd
      then Some (if slen = clen then "" else String.sub s (clen + 1) (slen - clen - 1))
      else None
  in
    match try_match_cmd "REJECTED" with | Some options -> Server_rejected options | None ->
       (match try_match_cmd "OK" with | Some o -> Server_ok o | None ->
          (match try_match_cmd "DATA" with | Some d -> Server_data d | None ->
             (match try_match_cmd "ERROR" with | Some _ -> Server_error | None ->
                Server_other s)))

(* Client parsing state and protocol state machine *)

type client_state =
  | Waiting_for_data
  | Waiting_for_ok
  | Waiting_for_reject

type auth_state =
  | Auth_in_progress
  | Auth_failed
  | Auth_succeeded of (* server address (guid) *) string * (* number of consumed bytes *) int

type cursor =
  | Line of char list
  | Line_CR of char list
  | Line_done of string

type client_context = {
  mutable state : client_state;
  mutable cursor : cursor;
  mechanism : mechanism;
}

type protocol_sender = string -> unit

let process_server_cmd ctxt sender s consumed =
  let send proto =
    sender (Printf.sprintf "%s\r\n" (client_proto_to_string proto)) in
  let server_proto = server_proto_of_string s in
  match ctxt.state with
    | Waiting_for_data ->
        (match server_proto with
           | Server_data data ->
               (match ctxt.mechanism#challenge data with
                  | Mech_continue resp ->
                      ctxt.state <- Waiting_for_data;
                      send (Client_data resp);
                      Auth_in_progress
                  | Mech_ok resp ->
                      ctxt.state <- Waiting_for_ok;
                      send (Client_data resp);
                      Auth_in_progress
                  | Mech_error ->
                      send (Client_error "Auth mechanism failure");
                      Auth_in_progress
               )
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_error ->
               ctxt.state <- Waiting_for_reject;
               send Client_cancel;
               Auth_in_progress
           | Server_ok server_addr ->
               send Client_begin;
               Auth_succeeded (server_addr, consumed)
           | Server_other _ ->
               send (Client_error "unrecognized protocol");
               Auth_in_progress
        )
    | Waiting_for_ok ->
        (match server_proto with
           | Server_ok server_addr ->
               send Client_begin;
               Auth_succeeded (server_addr, consumed)
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_error
           | Server_data _ ->
               ctxt.state <- Waiting_for_reject;
               send Client_cancel;
               Auth_in_progress
           | Server_other _ ->
               send (Client_error "unrecognized protocol");
               Auth_in_progress
        )
    | Waiting_for_reject ->
        (match server_proto with
           | Server_rejected _ ->
               (* TODO: attempt via a hitherto untried mechanism, before failing *)
               Auth_failed
           | Server_ok _
           | Server_data _
           | Server_error
           | Server_other _ ->
               Auth_failed
        )

(* Protocol initialization and parsing *)

let init_client_context mech_type sender =
  sender "\x00";
  let mech_str, mech =
    match mech_type with
      | External -> "EXTERNAL", (new external_mech : mechanism)
      | Anonymous -> "ANONYMOUS", (new anonymous_mech : mechanism) in
  let send init_resp =
    let proto =
      if init_resp = "" then Client_auth (mech_str, None)
      else Client_auth (mech_str, Some init_resp)
    in
      sender (Printf.sprintf "%s\r\n" (client_proto_to_string proto)) in
  let make_context init_state =
    {
      state = init_state;
      cursor = Line [];
      mechanism = mech;
    }
  in match mech#init with
    | Mech_continue init_resp ->
        send init_resp;
        make_context Waiting_for_data, Auth_in_progress
    | Mech_ok init_resp ->
        send init_resp;
        make_context Waiting_for_ok, Auth_in_progress
    | Mech_error ->
        (* Mechanisms shouldn't really have errors on initialization! *)
        make_context Waiting_for_reject, Auth_failed

let rev_string_of_chars cl =
  let len = List.length cl in
  let s = String.create len in
    ignore (List.fold_left (fun idx c -> s.[idx] <- c; idx - 1) (len - 1) cl);
    s

let parse_char ctxt c =
  match ctxt.cursor with
    | Line cl ->
        (match c with
           | '\r' -> ctxt.cursor <- Line_CR cl
           | '\n' -> raise_error (Unexpected_char c)
           | _    -> ctxt.cursor <- Line (c :: cl)
        )
    | Line_CR cl ->
        (match c with
           | '\r' -> ctxt.cursor <- Line_CR (c :: cl)
           | '\n' -> ctxt.cursor <- Line_done (rev_string_of_chars cl)
           | _    -> ctxt.cursor <- Line (c :: '\r' :: cl)
        )
    | Line_done _ ->
        raise_error (Internal_error "parse_char called on Line_done!")

let is_line_done ctxt =
  match ctxt.cursor with
    | Line_done _ -> true
    | _ -> false

exception Done of auth_state
let parse_input ctxt sender s start len =
  let iend = start + len in
  let rec helper ofs =
    let i = ref ofs in
      while not (is_line_done ctxt) && !i < iend do
        parse_char ctxt s.[!i];
        incr i;
      done;
      match ctxt.cursor with
        | Line_done s ->
            ctxt.cursor <- Line [];
            let result = process_server_cmd ctxt sender s (!i - start) in
              (match result with
                 | Auth_failed
                 | Auth_succeeded _ ->
                     raise (Done result)
                 | Auth_in_progress ->
                     helper !i
              )
        | _ ->
            Auth_in_progress
  in
    try helper start
    with Done result -> result
