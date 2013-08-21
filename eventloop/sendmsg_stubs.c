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

#define _GNU_SOURCE
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>

#ifdef SCM_CREDS                            /* BSD   */
#include <sys/un.h>
#define SCM_CRED_TYPE       SCM_CREDS
#define CRED_RECV_SOCKOPT   LOCAL_PEERCRED
#define CRED_STRUCT         cmsgcred
#define CRED_PID            cmcred_pid
#define CRED_UID            cmcred_uid
#define CRED_GID            cmcred_gid
#elif defined(SCM_CREDENTIALS)              /* Linux */
#define SCM_CRED_TYPE       SCM_CREDENTIALS
#define CRED_RECV_SOCKOPT   SO_PASSCRED
#define CRED_STRUCT         ucred
#define CRED_PID            pid
#define CRED_UID            uid
#define CRED_GID            gid
#else
#error "Platform not supported for credential passing."
#endif

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include "posix_stubs.h"

static int send_flag_table[] = {
    MSG_DONTROUTE, MSG_DONTWAIT, MSG_EOR, MSG_NOSIGNAL, MSG_OOB
};

static int recv_flag_table[] = {
    MSG_CMSG_CLOEXEC, MSG_DONTWAIT, MSG_OOB, MSG_PEEK, MSG_TRUNC, MSG_WAITALL
};

static int msg_flag_table[] = {
    MSG_EOR, MSG_TRUNC, MSG_CTRUNC, MSG_OOB
};

/* Two candidates for the OCaml C library. */
static int list_length (value list) {
    int len = 0;
    while (list != Val_int(0)) {
        len++;
        list = Field(list, 1);
    }
    return len;
}

/* socket option for receiving credentials */
CAMLprim value stub_set_recvcred(value fd, value flag) {
    CAMLparam2(fd, flag);
    int setting = Bool_val(flag);
    int ret = setsockopt(Int_val(fd), SOL_SOCKET, CRED_RECV_SOCKOPT, &setting, sizeof(setting));
    if (ret < 0)
        raise_unix_error(errno, "set_recvcred", "");
    CAMLreturn(Val_unit);
}

/* utility to compute amount of actual data in a cmsg. */
static int cmsg_data_len(const struct msghdr *msg, const struct cmsghdr *cmsg) {
    size_t data_len;
    char *msg_data_end  = (char *)msg->msg_control + msg->msg_controllen;
    char *cmsg_data_end = (char *)cmsg + cmsg->cmsg_len;
    if (cmsg->cmsg_len <= CMSG_LEN(0))
        return 0;
    data_len = cmsg->cmsg_len - CMSG_LEN(0);
    /* common case */
    if (cmsg_data_end <= msg_data_end)
        return data_len;
    /* if truncated, return only amount present */
    return (msg_data_end - ((char *)cmsg + CMSG_LEN(0)));
}

/* Main {send,recv}msg stubs and helpers. */

enum cmsg_tag {
    Cmsg_generic,
    Cmsg_scm_rights,
    Cmsg_scm_credentials,
};

static size_t vcmsg_data_len(value v) {
    switch (Tag_val(v)) {
    case Cmsg_generic:
        return caml_string_length(Field(v, 2));
    case Cmsg_scm_rights:
        return sizeof(int) * list_length(Field(v, 0));
    case Cmsg_scm_credentials:
        return sizeof(struct CRED_STRUCT);
    }
    return 0;
}

/* NOTE: This assumes the containing control buffer has enough space
   allocated for the cmsg, e.g. using vcsmg_space(). */
static void to_cmsg(value v, struct cmsghdr *cmsg) {
    struct CRED_STRUCT cred;
    int *fdp;
    switch (Tag_val(v)) {
    case Cmsg_generic:
        cmsg->cmsg_len   = CMSG_LEN(caml_string_length(Field(v, 2)));
        cmsg->cmsg_level = Int_val(Field(v, 0));
        cmsg->cmsg_type  = Int_val(Field(v, 1));
        memcpy(CMSG_DATA(cmsg), String_val(Field(v, 2)), caml_string_length(Field(v, 2)));
        break;
    case Cmsg_scm_rights:
        cmsg->cmsg_level = SOL_SOCKET;
        cmsg->cmsg_type  = SCM_RIGHTS;
        /* copy from fd list */
        fdp = (int *)CMSG_DATA(cmsg);
        v = Field(v, 0);
        cmsg->cmsg_len = CMSG_LEN(sizeof(int) * list_length(v));
        while (v != Val_int(0)) {
            *fdp++ = Int_val(Field(v, 0));
            v = Field(v, 1);
        }
        break;
    case Cmsg_scm_credentials:
        cmsg->cmsg_len   = CMSG_LEN(sizeof(cred));
        cmsg->cmsg_level = SOL_SOCKET;
        cmsg->cmsg_type  = SCM_CRED_TYPE;
        v = Field(v, 0);
        cred.CRED_PID = Int_val(Field(v, 0));
        cred.CRED_UID = Int_val(Field(v, 1));
        cred.CRED_GID = Int_val(Field(v, 2));
        memcpy(CMSG_DATA(cmsg), &cred, sizeof(cred));
        break;
    default:
        cmsg->cmsg_len   = CMSG_LEN(0);
        cmsg->cmsg_level = 0;
        cmsg->cmsg_type  = 0;
    }
}

CAMLprim value stub_sendmsg(value fd, value iovec_strings, value cmsgs, value send_flags) {
    CAMLparam4(fd, iovec_strings, cmsgs, send_flags);
    CAMLlocal1(list);
    int ret, flags;
    struct msghdr msg = {0};

    /* iovecs */
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
    /* cmsgs */
    list = cmsgs;
    while (list != Val_int(0)) {
        msg.msg_controllen += CMSG_SPACE(vcmsg_data_len(Field(list, 0)));
        list = Field(list, 1);
    }
    if (msg.msg_controllen > 0) {
        int cnt = 0;
        if ((msg.msg_control = (struct cmsghdr *) malloc(msg.msg_controllen)) == NULL)
            goto on_error;
        list = cmsgs;
        struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
        while (list != Val_int(0)) {
            to_cmsg(Field(list, 0), cmsg);
            cmsg = CMSG_NXTHDR(&msg, cmsg);
            list = Field(list, 1);
            cnt += 1;
        }
    }
    /* send flags */
    flags = caml_convert_flag_list(send_flags, send_flag_table);
    enter_blocking_section();
    ret = sendmsg(Int_val(fd), &msg, flags);
    leave_blocking_section();

 on_error:
    if (msg.msg_iov)     free(msg.msg_iov);
    if (msg.msg_control) free(msg.msg_control);

    if (ret < 0)
        raise_unix_error(errno, "sendmsg", "");
    CAMLreturn(Val_int(ret));
}

static value of_cmsg(const struct msghdr *msg, const struct cmsghdr *cmsg) {
    CAMLparam0();
    CAMLlocal2(vret, v);
    if (cmsg->cmsg_level == SOL_SOCKET) {
        struct CRED_STRUCT cred;
        int *fdp, *fd_end;
        switch (cmsg->cmsg_type) {
        case SCM_RIGHTS:
            fd_end = (int *)(CMSG_DATA(cmsg) + cmsg_data_len(msg, cmsg));
            fdp   = (int *)CMSG_DATA(cmsg);
            vret  = Val_int(0);
            while (fdp < fd_end) {
                v = caml_alloc(2, 0);
                Field(v, 0) = Val_int(*fdp);
                Field(v, 1) = vret;
                vret = v;
                fdp++;
            }
            v = vret;
            vret = caml_alloc(1, Cmsg_scm_rights);
            Field(vret, 0) = v;
            CAMLreturn(vret);

        case SCM_CRED_TYPE:
            memcpy(&cred, CMSG_DATA(cmsg), sizeof(cred));
            v = caml_alloc(3, 0);
            Field(v, 0) = Val_int(cred.CRED_PID);
            Field(v, 1) = Val_int(cred.CRED_UID);
            Field(v, 2) = Val_int(cred.CRED_GID);
            vret = caml_alloc(1, Cmsg_scm_credentials);
            Field(vret, 0) = v;
            CAMLreturn(vret);
        }
    }
    vret = caml_alloc(3, Cmsg_generic);
    Field(vret, 0) = Val_int(cmsg->cmsg_level);
    Field(vret, 1) = Val_int(cmsg->cmsg_type);
    Field(vret, 2) = caml_alloc_string(0); // avoid returning unknown data; FIXME
    CAMLreturn(vret);
}

CAMLprim value stub_recvmsg(value fd, value recv_flags) {
    CAMLparam1(fd);
    CAMLlocal3(vret, vlist, v);

    int ret, orig_flags, flags, sized;
    struct iovec io;
    struct msghdr msg = {0};
    static int cbufsize = 0;
    if (cbufsize == 0)
        cbufsize = sysconf(_SC_PAGESIZE);

    io.iov_len  = cbufsize;
    io.iov_base = malloc(io.iov_len);
    msg.msg_iov = &io;
    msg.msg_iovlen = 1;
    msg.msg_controllen = cbufsize;
    msg.msg_control = malloc(msg.msg_controllen);

    orig_flags = caml_convert_flag_list(recv_flags, recv_flag_table);

    /* Loop, peeking at the receive queue, until our recv buffers are
       correctly sized. */
    flags = orig_flags | MSG_PEEK;
    sized = 0;
 retry:
    enter_blocking_section();
    ret = recvmsg(Int_val(fd), &msg, flags);
    leave_blocking_section();

    if (ret < 0) {
        if (io.iov_base)     free(io.iov_base);
        if (msg.msg_control) free(msg.msg_control);
        raise_unix_error(errno, "recvmsg", "");
    } else if (io.iov_len < ret || msg.msg_flags & MSG_TRUNC) {
        io.iov_len *= 2;
        io.iov_base = realloc(io.iov_base, io.iov_len);
        goto retry;
    } else if (msg.msg_flags & MSG_CTRUNC) {
        msg.msg_controllen *= 2;
        msg.msg_control = realloc(msg.msg_control, msg.msg_controllen);
        goto retry;
    } else if (sized == 0) {
        /* Buffers are now correctly sized, no need to do internal
           peek anymore. */
        flags = orig_flags;
        sized = 1;
        goto retry;
    }

    vret = caml_alloc_tuple(3);
    Field(vret, 0) = Field(vret, 1) = Field(vret, 2) = Val_int(0);

    /* iovec */
    vlist = Val_int(0);
    if (ret > 0) {
        v = caml_alloc(2, 0);
        Field(v, 1) = vlist;
        Field(v, 0) = caml_alloc_string(ret);
        memmove(String_val(Field(v, 0)), io.iov_base, ret);
        vlist = v;
    }
    Field(vret, 0) = vlist;
    /* cmsgs */
    vlist = Val_int(0);
    if (msg.msg_controllen > 0) {
        struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
        while (cmsg) {
            v = caml_alloc(2, 0);
            Field(v, 0) = of_cmsg(&msg, cmsg);
            Field(v, 1) = vlist;
            cmsg = CMSG_NXTHDR(&msg, cmsg);
            vlist = v;
        }
    }
    Field(vret, 1) = vlist;
    /* msg_flags */
    Field(vret, 2) = make_flag_list(msg.msg_flags, msg_flag_table,
                                    sizeof(msg_flag_table)/sizeof(int));

    CAMLreturn(vret);
}
