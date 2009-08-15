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

val header_to_string : header -> string

type method_call = {
  method_call_flags : flag list;
  method_call_serial : int64;
  method_call_path : string;
  method_call_member : string;
  method_call_interface : string option;
  method_call_destination : string option;
  method_call_sender : string option;
  method_call_signature : Dbus_type.t list;
  method_call_payload : Dbus_value.t list;
}

type method_return = {
  method_return_flags : flag list;
  method_return_serial : int64;
  method_return_reply_serial : int64;
  method_return_destination : string option;
  method_return_sender : string option;
  method_return_signature : Dbus_type.t list;
  method_return_payload : Dbus_value.t list;
}

type error = {
  error_flags : flag list;
  error_serial : int64;
  error_name : string;
  error_reply_serial : int64;
  error_destination : string option;
  error_sender : string option;
  error_signature : Dbus_type.t list;
  error_payload : Dbus_value.t list;
}

type signal = {
  signal_flags : flag list;
  signal_serial : int64;
  signal_path : string;
  signal_interface : string;
  signal_member : string;
  signal_destination : string option;
  signal_sender : string option;
  signal_signature : Dbus_type.t list;
  signal_payload : Dbus_value.t list;
}

type msg_type =
  | Msg_type_method_call
  | Msg_type_method_return
  | Msg_type_error
  | Msg_type_signal

val msg_type_to_string : msg_type -> string

type t =
  | Msg_method_call of method_call
  | Msg_method_return of method_return
  | Msg_error of error
  | Msg_signal of signal

(* Accessors *)

val get_type : t -> msg_type
val get_flags : t -> flag list
val get_serial : t -> int64
val get_destination : t -> string option
val get_sender : t -> string option
val get_signature : t -> Dbus_type.t list
val get_payload : t -> Dbus_value.t list
val get_headers : t -> (header * (Dbus_type.t * Dbus_value.t)) list

(* Constructors *)

val method_call : ?flags:flag list -> serial:int64 -> ?destination:string
  -> ?interface:string -> ?path:string -> member:string
  -> signature:Dbus_type.t list -> Dbus_value.t list
  -> t

val method_return : ?flags:flag list -> serial:int64 -> ?destination:string
  -> reply_serial:int64
  -> signature:Dbus_type.t list -> Dbus_value.t list
  -> t

val error : ?flags:flag list -> serial:int64 -> ?destination:string
  -> name:string -> reply_serial:int64
  -> signature:Dbus_type.t list -> Dbus_value.t list
  -> t

val signal : ?flags:flag list -> serial:int64 -> ?destination:string
  -> interface:string -> path:string -> member:string
  -> signature:Dbus_type.t list -> Dbus_value.t list
  -> t

(* Pretty printer *)
val pr_msg : Format.formatter -> t -> unit
