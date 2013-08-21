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

type cap_right =
(* General file I/O. *)
| CAP_READ
| CAP_WRITE
| CAP_SEEK
| CAP_PREAD
| CAP_PWRITE
| CAP_MMAP
| CAP_MMAP_R
| CAP_MMAP_W
| CAP_MMAP_X
| CAP_MMAP_RW
| CAP_MMAP_RX
| CAP_MMAP_WX
| CAP_MMAP_RWX
| CAP_CREATE
| CAP_FEXECVE
| CAP_FSYNC
| CAP_FTRUNCATE
(* VFS methods. *)
| CAP_FCHDIR
| CAP_FCHFLAGS
| CAP_CHFLAGSAT
| CAP_FCHMOD
| CAP_FCHMODAT
| CAP_FCHOWN
| CAP_FCHOWNAT
| CAP_FCNTL
| CAP_FLOCK
| CAP_FPATHCONF
| CAP_FSCK
| CAP_FSTAT
| CAP_FSTATAT
| CAP_FSTATFS
| CAP_FUTIMES
| CAP_FUTIMESAT
| CAP_LINKAT
| CAP_MKDIRAT
| CAP_MKFIFOAT
| CAP_MKNODAT
| CAP_RENAMEAT
| CAP_SYMLINKAT
| CAP_UNLINKAT
(* lookups - used to constrain *at() calls. *)
| CAP_LOOKUP
(* extended attributes. *)
| CAP_EXTATTR_DELETE
| CAP_EXTATTR_GET
| CAP_EXTATTR_LIST
| CAP_EXTATTR_SET
(* Access Control Lists. *)
| CAP_ACL_CHECK
| CAP_ACL_DELETE
| CAP_ACL_GET
| CAP_ACL_SET
(* Socket operations. *)
| CAP_ACCEPT
| CAP_BIND
| CAP_CONNECT
| CAP_GETPEERNAME
| CAP_GETSOCKNAME
| CAP_GETSOCKOPT
| CAP_LISTEN
| CAP_PEELOFF
| CAP_RECV
| CAP_SEND
| CAP_SETSOCKOPT
| CAP_SHUTDOWN
| CAP_SOCK_CLIENT
| CAP_SOCK_SERVER
(* Mandatory Access Control. *)
| CAP_MAC_GET
| CAP_MAC_SET
(* kqueue events. *)
| CAP_POLL_EVENT
| CAP_POST_EVENT
(* Process management via process descriptors. *)
| CAP_PDGETPID
| CAP_PDWAIT
| CAP_PDKILL
(* Allowing bindat(2) and connectat(2) syscalls on a * directory
   descriptor.  *)
| CAP_BINDAT
| CAP_CONNECTAT

type fcntl_flag =
| CAP_FCNTL_GETFL
| CAP_FCNTL_SETFL
| CAP_FCNTL_GETOWN
| CAP_FCNTL_SETOWN
| CAP_FCNTL_ALL

external is_sandboxed : unit -> bool = "stub_cap_sandboxed"

external cap_enter : unit -> unit = "stub_cap_enter"

external cap_rights_limit : Unix.file_descr -> cap_right list -> unit = "stub_cap_rights_limit"
external cap_rights_get : Unix.file_descr -> cap_right list = "stub_cap_rights_get"

let cap_fcntls_limit fd flist = ()
let cap_fcntls_get fd = []
