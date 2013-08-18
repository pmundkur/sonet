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

(* credentials *)

type cred = {
  pid : int;    (* PID of sending process. *)
  uid : int;    (* UID of sending process. *)
  gid : int;    (* GID of sending process. *)
}

val getcred : unit -> cred      (* credentials of caller *)
val set_passcred : Unix.file_descr -> bool -> unit

(* Control messages that can be sent and received via sendmsg(). *)

type proto_level = int
type proto_type  = int
type cmsg =
| Cmsg_generic of proto_level * proto_type * string
| Cmsg_scm_rights of Unix.file_descr list
| Cmsg_scm_credentials of cred

(* TODO: check *BSD for flag support. *)
type send_flag =
| SEND_CONFIRM
| SEND_DONTROUTE
| SEND_DONTWAIT
| SEND_EOR
| SEND_MORE
| SEND_NOSIGNAL
| SEND_OOB

type recv_flag =
| RECV_CMSG_CLOEXEC
| RECV_DONTWAIT
| RECV_ERRQUEUE
| RECV_OOB
| RECV_PEEK
| RECV_TRUNC
| RECV_WAITALL

type msg_flag =
| MSG_EOR
| MSG_TRUNC
| MSG_CTRUNC
| MSG_OOB
| MSG_ERRQUEUE

val send_flag_name : send_flag -> string
val recv_flag_name : recv_flag -> string
val msg_flag_name  :  msg_flag -> string

(* TODO: add the sock address in msg_name. *)
type msg = {
  msg_iovec : string list;      (* will contain at most one buffer on recvmsg *)
  msg_cmsgs : cmsg list;
  msg_flags : msg_flag list;    (* ignored on sendmsg *)
}

val sendmsg : Unix.file_descr -> msg -> send_flag list -> int
val recvmsg : Unix.file_descr -> recv_flag list -> msg
