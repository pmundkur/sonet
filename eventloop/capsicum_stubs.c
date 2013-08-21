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

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include "posix_stubs.h"

#ifndef __FreeBSD__

CAMLprim value stub_cap_sandboxed(value unit) {
    CAMLparam1(unit);
    raise_unix_error(ENOTSUP, "cap_sandboxed", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_enter(value unit) {
    CAMLparam1(unit);
    raise_unix_error(ENOTSUP, "cap_enter", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_rights_limit(value fd, value rights) {
    CAMLparam2(fd, rights);
    raise_unix_error(ENOTSUP, "cap_rights_limit", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_rights_get(value fd) {
    CAMLparam2(fd, rights);
    raise_unix_error(ENOTSUP, "cap_rights_get", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_fcntls_limit(value fd, value rights) {
    CAMLparam2(fd, rights);
    raise_unix_error(ENOTSUP, "cap_fcntls_limit", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_fcntls_get(value fd) {
    CAMLparam1(fd);
    raise_unix_error(ENOTSUP, "cap_fcntls_get", "");
    CAMLreturn(Val_unit);
}

#else

#include <sys/capability.h>
#include <stdbool.h>

static cap_rights_t rights_table[] = {
    /* General file I/O. */
    CAP_READ, CAP_WRITE, CAP_SEEK, CAP_PREAD, CAP_PWRITE,
    CAP_MMAP, CAP_MMAP_R, CAP_MMAP_W, CAP_MMAP_X,
    CAP_MMAP_RW, CAP_MMAP_RX, CAP_MMAP_WX, CAP_MMAP_RWX,
    CAP_CREATE, CAP_FEXECVE, CAP_FSYNC, CAP_FTRUNCATE,
    /* VFS methods. */
    CAP_FCHDIR, CAP_FCHFLAGS, CAP_CHFLAGSAT, CAP_FCHMOD, CAP_FCHMODAT,
    CAP_FCHOWN, CAP_FCHOWNAT, CAP_FCNTL, CAP_FLOCK, CAP_FPATHCONF, CAP_FSCK,
    CAP_FSTAT, CAP_FSTATAT, CAP_FSTATFS, CAP_FUTIMES, CAP_FUTIMESAT,
    CAP_LINKAT, CAP_MKDIRAT, CAP_MKFIFOAT, CAP_MKNODAT, CAP_RENAMEAT, CAP_SYMLINKAT, CAP_UNLINKAT,
    /* Lookups - used to constrain *at() calls. */
    CAP_LOOKUP,
    /* Extended attributes. */
    CAP_EXTATTR_DELETE, CAP_EXTATTR_GET, CAP_EXTATTR_LIST, CAP_EXTATTR_SET,
    /* Access Control Lists. */
    CAP_ACL_CHECK, CAP_ACL_DELETE, CAP_ACL_GET, CAP_ACL_SET,
    /* Socket operations. */
    CAP_ACCEPT, CAP_BIND, CAP_CONNECT,
    CAP_GETPEERNAME, CAP_GETSOCKNAME, CAP_GETSOCKOPT, CAP_LISTEN, CAP_PEELOFF,
    CAP_RECV, CAP_SEND, CAP_SETSOCKOPT, CAP_SHUTDOWN,
    CAP_SOCK_CLIENT, CAP_SOCK_SERVER,
    /* Mandatory Access Control. */
    CAP_MAC_GET, CAP_MAC_SET,
    /* kqueue events. */
    CAP_POLL_EVENT, CAP_POST_EVENT,
    /* Process management via process descriptors. */
    CAP_PDGETPID, CAP_PDWAIT, CAP_PDKILL,
    /*
     * Rights that allow to use bindat(2) and connectat(2) syscalls on a
     * directory descriptor.
     */
    CAP_BINDAT, CAP_CONNECTAT
};

uint32_t fcntl_rights_table[] = {
    CAP_FCNTL_GETFL, CAP_FCNTL_SETFL,
    CAP_FCNTL_GETOWN, CAP_FCNTL_SETOWN,
    CAP_FCNTL_ALL
};

cap_rights_t convert_llong_flag_list(value list, cap_rights_t *flags) {
    cap_rights_t res = 0;
    while (list != Val_int(0)) {
        res |= flags[Int_val(Field(list, 0))];
        list = Field(list, 1);
    }
    return res;
}

value make_llong_flag_list(cap_rights_t f, cap_rights_t *flags, int n) {
    CAMLparam0();
    CAMLlocal2(vlist, v);
    int i;

    vlist = Val_int(0);
    for (i = 0; i < n; i++) {
        if ((f & flags[i]) == flags[i]) {
            v = caml_alloc(2, 0);
            Field(v, 0) = Val_int(i);
            Field(v, 1) = vlist;
            vlist = v;
            f &= ~flags[i];
        }
    }
    CAMLreturn(vlist);
}


CAMLprim value stub_cap_sandboxed(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(ret);
    ret = cap_sandboxed() ? Val_true : Val_false;
    CAMLreturn(ret);
}

CAMLprim value stub_cap_enter(value unit) {
    CAMLparam1(unit);
    if (cap_enter())
        raise_unix_error(errno, "cap_enter", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_rights_limit(value fd, value vrights) {
    CAMLparam2(fd, vrights);
    int rights = convert_llong_flag_list(vrights, rights_table);
    if (cap_rights_limit(Int_val(fd), rights))
        raise_unix_error(errno, "cap_rights_limit", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_rights_get(value fd) {
    CAMLparam1(fd);
    CAMLlocal1(vrights);
    cap_rights_t rights;
    if (cap_rights_get(Val_int(fd), &rights))
        raise_unix_error(errno, "cap_rights_get", "");
    vrights = make_llong_flag_list(rights, rights_table, sizeof(rights_table)/sizeof(cap_rights_t));
    CAMLreturn(vrights);
}

/* Commented out due to signedness issues. */
#if 0
CAMLprim value stub_cap_fcntls_limit(value fd, value vrights) {
    CAMLparam2(fd, vrights);
    int rights = caml_convert_flag_list(vrights, fcntl_rights_table);
    if (cap_fcntls_limit(Int_val(fd), rights))
        raise_unix_error(errno, "cap_fcntls_limit", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_cap_fcntls_get(value fd) {
    CAMLparam1(fd);
    CAMLlocal1(vrights);
    uint32_t fcntlrights;
    if (cap_fcntls_get(Int_val(fd), &fcntlrights))
        raise_unix_error(errno, "cap_fcntls_get", "");
    vrights = make_flag_list(rights, fcntls_rights_table, sizeof(fcntls_rights_table)/sizeof(uint32_t));
    CAMLreturn(vrights);
}
#endif

#endif
