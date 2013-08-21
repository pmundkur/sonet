(*
 * Copyright (c) 2013      SRI International, Inc.
 * All rights reserved.
 *
 * This software was developed by SRI International and the University
 * of Cambridge Computer Laboratory under DARPA/AFRL contract
 * (FA8750-11-C-0249) ("MRC2"), as part of the DARPA MRC research
 * programme.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type cred = {
  pid : int;    (* PID of sending process. *)
  uid : int;    (* UID of sending process. *)
  gid : int;    (* GID of sending process. *)
}

let getcred () =
  {pid = Unix.getpid ();
   uid = Unix.getuid ();
   gid = Unix.getgid ()}

external set_passcred_impl : Unix.file_descr -> bool -> unit = "stub_set_passcred"

let set_passcred fd bool =
  set_passcred_impl fd bool

type proto_level = int
type proto_type  = int
type cmsg =
| Cmsg_generic of proto_level * proto_type * string
| Cmsg_scm_rights of Unix.file_descr list
| Cmsg_scm_credentials of cred

type send_flag =
| SEND_DONTROUTE
| SEND_DONTWAIT
| SEND_EOR
| SEND_NOSIGNAL
| SEND_OOB

type recv_flag =
| RECV_CMSG_CLOEXEC
| RECV_DONTWAIT
| RECV_OOB
| RECV_PEEK
| RECV_TRUNC
| RECV_WAITALL

type msg_flag =
| MSG_EOR
| MSG_TRUNC
| MSG_CTRUNC
| MSG_OOB

let send_flag_name = function
  | SEND_DONTROUTE  -> "SEND_DONTROUTE"
  | SEND_DONTWAIT   -> "SEND_DONTWAIT"
  | SEND_EOR        -> "SEND_EOR"
  | SEND_NOSIGNAL   -> "SEND_NOSIGNAL"
  | SEND_OOB        -> "SEND_OOB"

let recv_flag_name = function
  | RECV_CMSG_CLOEXEC -> "RECV_CMSG_CLOEXEC"
  | RECV_DONTWAIT     -> "RECV_DONTWAIT"
  | RECV_OOB          -> "RECV_OOB"
  | RECV_PEEK         -> "RECV_PEEK"
  | RECV_TRUNC        -> "RECV_TRUNC"
  | RECV_WAITALL      -> "RECV_WAITALL"

let msg_flag_name = function
  | MSG_EOR        -> "MSG_EOR"
  | MSG_TRUNC      -> "MSG_TRUNC"
  | MSG_CTRUNC     -> "MSG_CTRUNC"
  | MSG_OOB        -> "MSG_OOB"

type msg = {
  msg_iovec : string list;
  msg_cmsgs : cmsg list;
  msg_flags : msg_flag list;
}

external sendmsg_impl : Unix.file_descr -> string list -> cmsg list -> send_flag list -> int
  = "stub_sendmsg"

external recvmsg_impl : Unix.file_descr -> recv_flag list -> string list * cmsg list * msg_flag list
  = "stub_recvmsg"

let sendmsg fd msg flags =
  sendmsg_impl fd msg.msg_iovec msg.msg_cmsgs flags

let recvmsg fd recv_flags =
  let iovec, cmsgs, flags = recvmsg_impl fd recv_flags in
  {msg_iovec = iovec; msg_cmsgs = cmsgs; msg_flags = flags}
