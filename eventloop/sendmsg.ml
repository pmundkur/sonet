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

type proto_level = int
type proto_type  = int
type cmsg =
(* Always keep Cmsg_generic as the first variant. *)
| Cmsg_generic of proto_level * proto_type * string

type send_flag =
| SEND_CMSG_CLOEXEC
| SEND_DONTWAIT
| SEND_ERRQUEUE
| SEND_OOB
| SEND_PEEK
| SEND_TRUNC
| SEND_WAITALL

type recv_flag =
| RECV_EOR
| RECV_TRUNC
| RECV_CTRUNC
| RECV_OOB
| RECV_ERRQUEUE

type msg = {
  msg_iovec : string list;
  msg_cmsgs : cmsg list;
  msg_flags : recv_flag list;
}

external sendmsg_impl : Unix.file_descr -> string list -> cmsg list -> send_flag list -> int
  = "stub_sendmsg"

external recvmsg_impl : Unix.file_descr -> string list * cmsg list * recv_flag list
  = "stub_recvmsg"

let sendmsg fd msg flags =
  sendmsg_impl fd msg.msg_iovec msg.msg_cmsgs flags

let recvmsg fd =
  let iovec, cmsgs, flags = recvmsg_impl fd in
  {msg_iovec = iovec; msg_cmsgs = cmsgs; msg_flags = flags}
