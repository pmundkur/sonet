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

module T = Dbus_type
module V = Dbus_value

type flag =
  | Msg_flag_no_reply_expected
  | Msg_flag_no_auto_start

type header =
  | Hdr_path
  | Hdr_interface
  | Hdr_member
  | Hdr_error_name
  | Hdr_reply_serial
  | Hdr_destination
  | Hdr_sender
  | Hdr_signature

let header_to_string = function
  | Hdr_path            -> "PATH"
  | Hdr_interface       -> "INTERFACE"
  | Hdr_member          -> "MEMBER"
  | Hdr_error_name      -> "ERROR_NAME"
  | Hdr_reply_serial    -> "REPLY_SERIAL"
  | Hdr_destination     -> "DESTINATION"
  | Hdr_sender          -> "SENDER"
  | Hdr_signature       -> "SIGNATURE"

type msg_type =
  | Msg_type_method_call
  | Msg_type_method_return
  | Msg_type_error
  | Msg_type_signal

let msg_type_to_string = function
  | Msg_type_method_call    -> "METHOD_CALL"
  | Msg_type_method_return  -> "METHOD_RETURN"
  | Msg_type_error          -> "ERROR"
  | Msg_type_signal         -> "SIGNAL"

type method_call = {
  method_call_flags : flag list;
  method_call_serial : int64;
  method_call_path : string;
  method_call_member : string;
  method_call_interface : string option;
  method_call_destination : string option;
  method_call_sender : string option;
  method_call_signature : T.t list;
  method_call_payload : V.t list;
}

type method_return = {
  method_return_flags : flag list;
  method_return_serial : int64;
  method_return_reply_serial : int64;
  method_return_destination : string option;
  method_return_sender : string option;
  method_return_signature : T.t list;
  method_return_payload : V.t list;
}

type error = {
  error_flags : flag list;
  error_serial : int64;
  error_name : string;
  error_reply_serial : int64;
  error_destination : string option;
  error_sender : string option;
  error_signature : T.t list;
  error_payload : V.t list;
}

type signal = {
  signal_flags : flag list;
  signal_serial : int64;
  signal_path : string;
  signal_interface : string;
  signal_member : string;
  signal_destination : string option;
  signal_sender : string option;
  signal_signature : T.t list;
  signal_payload : V.t list;
}

type t =
  | Msg_method_call of method_call
  | Msg_method_return of method_return
  | Msg_error of error
  | Msg_signal of signal

let get_type m =
  match m with
    | Msg_method_call _ -> Msg_type_method_call
    | Msg_method_return _ -> Msg_type_method_return
    | Msg_error _ -> Msg_type_error
    | Msg_signal _ -> Msg_type_signal

let get_flags m =
  match m with
    | Msg_method_call m -> m.method_call_flags
    | Msg_method_return m -> m.method_return_flags
    | Msg_error m -> m.error_flags
    | Msg_signal m -> m.signal_flags

let get_serial m =
  match m with
    | Msg_method_call m -> m.method_call_serial
    | Msg_method_return m -> m.method_return_serial
    | Msg_error m -> m.error_serial
    | Msg_signal m -> m.signal_serial

let get_destination m =
  match m with
    | Msg_method_call m -> m.method_call_destination
    | Msg_method_return m -> m.method_return_destination
    | Msg_error m -> m.error_destination
    | Msg_signal m -> m.signal_destination

let get_sender m =
  match m with
    | Msg_method_call m -> m.method_call_sender
    | Msg_method_return m -> m.method_return_sender
    | Msg_error m -> m.error_sender
    | Msg_signal m -> m.signal_sender

let get_signature m =
  match m with
    | Msg_method_call m -> m.method_call_signature
    | Msg_method_return m -> m.method_return_signature
    | Msg_error m -> m.error_signature
    | Msg_signal m -> m.signal_signature

let get_payload m =
  match m with
    | Msg_method_call m -> m.method_call_payload
    | Msg_method_return m -> m.method_return_payload
    | Msg_error m -> m.error_payload
    | Msg_signal m -> m.signal_payload

let get_headers m =
  let add_destination hdrs =
    match get_destination m with
      | None   -> hdrs
      | Some d -> (Hdr_destination, (T.T_base T.B_string, V.V_string d)) :: hdrs in
  let add_sender hdrs =
    match get_sender m with
      | None   -> hdrs
      | Some s -> (Hdr_sender, (T.T_base T.B_string, V.V_string s)) :: hdrs in
  let add_signature hdrs =
    match get_signature m with
      | [] -> hdrs
      | tl -> (Hdr_signature, (T.T_base T.B_signature, V.V_signature tl)) :: hdrs in
  let required_headers =
    match m with
      | Msg_method_call mc -> [
          Hdr_path, (T.T_base T.B_object_path, V.V_object_path mc.method_call_path);
          Hdr_member, (T.T_base T.B_string, V.V_string mc.method_call_member);
        ] @ (match mc.method_call_interface with
               | None   -> []
               | Some i -> [ Hdr_interface, (T.T_base T.B_string, V.V_string i) ]
            )
      | Msg_method_return mr -> [
          Hdr_reply_serial, (T.T_base T.B_uint32, V.V_uint32 mr.method_return_reply_serial);
        ]
      | Msg_error e -> [
          Hdr_error_name, (T.T_base T.B_string, V.V_string e.error_name);
          Hdr_reply_serial, (T.T_base T.B_uint32, V.V_uint32 e.error_reply_serial);
        ]
      | Msg_signal s -> [
          Hdr_path, (T.T_base T.B_object_path, V.V_object_path s.signal_path);
          Hdr_interface, (T.T_base T.B_string, V.V_string s.signal_interface);
        ]
  in
    add_signature (add_sender (add_destination required_headers))

let method_call ?(flags=[]) ~serial ?destination
    ?interface ?(path="/") ~member
    ~signature payload
    =
  V.type_check_args signature payload;
  Msg_method_call {
    method_call_flags = flags;
    method_call_serial = serial;
    method_call_path = path;
    method_call_member = member;
    method_call_interface = interface;
    method_call_destination = destination;
    method_call_sender = None;
    method_call_signature = signature;
    method_call_payload = payload;
  }

let method_return ?(flags=[]) ~serial ?destination
    ~reply_serial
    ~signature payload
    =
  V.type_check_args signature payload;
  Msg_method_return {
    method_return_flags = flags;
    method_return_serial = serial;
    method_return_reply_serial = reply_serial;
    method_return_destination = destination;
    method_return_sender = None;
    method_return_signature = signature;
    method_return_payload = payload;
  }

let error ?(flags=[]) ~serial ?destination
    ~name ~reply_serial
    ~signature payload
    =
  V.type_check_args signature payload;
  Msg_error {
    error_flags = flags;
    error_serial = serial;
    error_name = name;
    error_reply_serial = reply_serial;
    error_destination = destination;
    error_sender = None;
    error_signature = signature;
    error_payload = payload;
  }

let signal ?(flags=[]) ~serial ?destination
    ~interface ~path ~member
    ~signature payload
    =
  V.type_check_args signature payload;
  Msg_signal {
    signal_flags = flags;
    signal_serial = serial;
    signal_path = path;
    signal_interface = interface;
    signal_member = member;
    signal_destination = destination;
    signal_sender = None;
    signal_signature = signature;
    signal_payload = payload;
  }

let pr_string_opt opt_s =
  match opt_s with
    | None -> ""
    | Some s -> s

let pr_method_call ff mc =
  Format.fprintf ff "@[<v 4>  <Method_call@,";
  Format.fprintf ff "serial       = %Ld@," mc.method_call_serial;
  Format.fprintf ff "path         = %s@,"  mc.method_call_path;
  Format.fprintf ff "member       = %s@,"  mc.method_call_member;
  Format.fprintf ff "interface    = %s@,"  (pr_string_opt mc.method_call_interface);
  Format.fprintf ff "destination  = %s@,"  (pr_string_opt mc.method_call_destination);
  Format.fprintf ff "sender       = %s@,"  (pr_string_opt mc.method_call_sender);
  Format.fprintf ff "signature    = %s@,"  (T.signature_of_types mc.method_call_signature);
  Format.fprintf ff "@[<v 2>payload =@,";
  List.iter (fun v -> V.pr_value ff v; Format.fprintf ff "@,") mc.method_call_payload;
  Format.fprintf ff "@]@,>@]@,"

let pr_method_return ff mr =
  Format.fprintf ff "@[<v 4>  <Method_return@,";
  Format.fprintf ff "serial       = %Ld@," mr.method_return_serial;
  Format.fprintf ff "reply_serial = %Ld@," mr.method_return_reply_serial;
  Format.fprintf ff "destination  = %s@,"  (pr_string_opt mr.method_return_destination);
  Format.fprintf ff "sender       = %s@,"  (pr_string_opt mr.method_return_sender);
  Format.fprintf ff "signature    = %s@,"  (T.signature_of_types mr.method_return_signature);
  Format.fprintf ff "@[<v 2>payload =@,";
  List.iter (fun v -> V.pr_value ff v; Format.fprintf ff "@,") mr.method_return_payload;
  Format.fprintf ff "@]@,>@]@,"

let pr_error ff er =
  Format.fprintf ff "@[<v 4>  <Error@,";
  Format.fprintf ff "serial       = %Ld@," er.error_serial;
  Format.fprintf ff "name         = %s@,"  er.error_name;
  Format.fprintf ff "reply_serial = %Ld@," er.error_reply_serial;
  Format.fprintf ff "destination  = %s@,"  (pr_string_opt er.error_destination);
  Format.fprintf ff "sender       = %s@,"  (pr_string_opt er.error_sender);
  Format.fprintf ff "signature    = %s@,"  (T.signature_of_types er.error_signature);
  Format.fprintf ff "@[<v 2>payload =@,";
  List.iter (fun v -> V.pr_value ff v; Format.fprintf ff "@,") er.error_payload;
  Format.fprintf ff "@]@,>@]@,"

let pr_signal ff sg =
  Format.fprintf ff "@[<v 4>  <Signal@,";
  Format.fprintf ff "serial       = %Ld@," sg.signal_serial;
  Format.fprintf ff "path         = %s@,"  sg.signal_path;
  Format.fprintf ff "interface    = %s@,"  sg.signal_interface;
  Format.fprintf ff "member       = %s@,"  sg.signal_member;
  Format.fprintf ff "destination  = %s@,"  (pr_string_opt sg.signal_destination);
  Format.fprintf ff "sender       = %s@,"  (pr_string_opt sg.signal_sender);
  Format.fprintf ff "signature    = %s@,"  (T.signature_of_types sg.signal_signature);
  Format.fprintf ff "@[<v 2>payload =@,";
  List.iter (fun v -> V.pr_value ff v; Format.fprintf ff "@,") sg.signal_payload;
  Format.fprintf ff "@]@,>@]@,"

let pr_msg ff = function
  | Msg_method_call mc   -> pr_method_call ff mc
  | Msg_method_return mr -> pr_method_return ff mr
  | Msg_error er         -> pr_error ff er
  | Msg_signal sg        -> pr_signal ff sg
