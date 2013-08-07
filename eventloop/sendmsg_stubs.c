/*
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
 */

#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

CAMLprim value stub_sendmsg(value fd, value iovec_strings, value cmsgs, value send_flags) {
    CAMLparam4(fd, iovec_strings, cmsgs, send_flags);
    CAMLreturn(Val_int(-1));
}

CAMLprim value stub_recvmsg(value fd) {
    CAMLparam1(fd);
    CAMLreturn(Val_unit);
}
