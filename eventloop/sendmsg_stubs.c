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

#include "posix_stubs.h"

static int send_flag_table[] = {
    MSG_CMSG_CLOEXEC, MSG_DONTWAIT, MSG_ERRQUEUE, MSG_OOB, MSG_PEEK, MSG_TRUNC, MSG_WAITALL
};

/*
static int recv_flag_table[] = {
    MSG_EOR, MSG_TRUNC, MSG_CTRUNC, MSG_OOB, MSG_ERRQUEUE
};
*/

#include <string.h>
#include <stdio.h>

CAMLprim value stub_sendmsg(value fd, value iovec_strings, value cmsgs, value send_flags) {
    CAMLparam4(fd, iovec_strings, cmsgs, send_flags);
    CAMLlocal1(list);
    int ret, flags;
    struct msghdr msg = {0};

    /* deal with iovecs */
    list = iovec_strings;
    while (list != Val_int(0)) {
        msg.msg_iovlen++;
        list = Field(list, 1);
    }
    if (msg.msg_iovlen > 0) {
        if ((msg.msg_iov = (struct iovec *) malloc(msg.msg_iovlen * sizeof(struct iovec))) == NULL)
            goto on_error;

        msg.msg_iovlen = 0;
        list = iovec_strings;
        while (list != Val_int(0)) {
            msg.msg_iov[msg.msg_iovlen].iov_base = String_val(Field(list, 0));
            msg.msg_iov[msg.msg_iovlen].iov_len  = Wosize_val(Field(list, 0));
            msg.msg_iovlen++;
            list = Field(list, 1);
        }
    }
    /* deal with cmsgs */
    list = cmsgs;
    while (list != Val_int(0)) {
        value v = Field(list, 0);
        msg.msg_controllen += CMSG_SPACE(caml_string_length(Field(v, 2)));
        list = Field(list, 1);
    }
    if (msg.msg_controllen > 0) {
        if ((msg.msg_control = (struct cmsghdr *) malloc(msg.msg_controllen)) == NULL)
            goto on_error;

        list = cmsgs;
        struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
        while (list != Val_int(0)) {
            value v = Field(list, 0);
            /* cmsg_len includes size of cmsghdr */
            cmsg->cmsg_len   = CMSG_LEN(Wosize_val(Field(v, 2)));
            cmsg->cmsg_level = Int_val(Field(v, 0));
            cmsg->cmsg_type  = Int_val(Field(v, 1));
            memcpy(CMSG_DATA(cmsg), String_val(Field(v, 2)), caml_string_length(Field(v, 2)));
            cmsg = CMSG_NXTHDR(&msg, cmsg);
            list = Field(list, 1);
        }
    }
    /* deal with send flags */
    flags = caml_convert_flag_list(send_flags, send_flag_table);

    enter_blocking_section();
    ret = sendmsg(Int_val(fd), &msg, flags);
    leave_blocking_section();

 on_error:
    if (msg.msg_iov)     free(msg.msg_iov);
    if (msg.msg_control) free(msg.msg_control);

    if (ret == -1)
        raise_unix_error(errno, "sendmsg", "");
    CAMLreturn(Val_int(ret));
}

CAMLprim value stub_recvmsg(value fd) {
    CAMLparam1(fd);
    CAMLreturn(Val_unit);
}
