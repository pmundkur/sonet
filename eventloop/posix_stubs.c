/*
 * Copyright (C) 2013      Prashanth Mundkur
 * Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>
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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "eventloop.h"

static int error_table[] = {
    E2BIG, EACCES, EAGAIN, EBADF, EBUSY, ECHILD, EDEADLK, EDOM,
    EEXIST, EFAULT, EFBIG, EINTR, EINVAL, EIO, EISDIR, EMFILE, EMLINK,
    ENAMETOOLONG, ENFILE, ENODEV, ENOENT, ENOEXEC, ENOLCK, ENOMEM, ENOSPC,
    ENOSYS, ENOTDIR, ENOTEMPTY, ENOTTY, ENXIO, EPERM, EPIPE, ERANGE,
    EROFS, ESPIPE, ESRCH, EXDEV, EWOULDBLOCK, EINPROGRESS, EALREADY,
    ENOTSOCK, EDESTADDRREQ, EMSGSIZE, EPROTOTYPE, ENOPROTOOPT,
    EPROTONOSUPPORT, ESOCKTNOSUPPORT, EOPNOTSUPP, EPFNOSUPPORT,
    EAFNOSUPPORT, EADDRINUSE, EADDRNOTAVAIL, ENETDOWN, ENETUNREACH,
    ENETRESET, ECONNABORTED, ECONNRESET, ENOBUFS, EISCONN, ENOTCONN,
    ESHUTDOWN, ETOOMANYREFS, ETIMEDOUT, ECONNREFUSED, EHOSTDOWN,
    EHOSTUNREACH, ELOOP, EOVERFLOW
};

// TODO: hide this symbol from leaking out of the lib.
void raise_unix_error(int errnum, char *fn_name, char *fn_param) {
    CAMLparam0();
    CAMLlocal2(v_fn_name, v_fn_param);
    CAMLlocalN(ea_vec, 3);

    int i;

    static value *caml_unix_exc_constr = NULL;
    if (NULL == caml_unix_exc_constr) {
        caml_unix_exc_constr = caml_named_value(UNIX_EXCEPTION_NAME);
        if (NULL == caml_unix_exc_constr)
            invalid_argument("Exception Unix.Unix_error not initialized, please link unix.cma");
    }

    v_fn_name = (NULL == fn_name) ? Atom(String_tag) : caml_copy_string(fn_name);
    v_fn_param = (NULL == fn_param) ? Atom(String_tag) : caml_copy_string(fn_param);

    ea_vec[0] = Val_int(-1);
    for (i = 0; i < sizeof(error_table)/sizeof(int); i++) {
        if (errnum == error_table[i]) {
            ea_vec[0] = Val_int(i);
            break;
        }
    }
    if (Val_int(-1) == ea_vec[0]) {
        ea_vec[0] = alloc_small(1, 0);
        Field(ea_vec[0], 0) = Val_int(errnum);
    }
    ea_vec[1] = v_fn_name;
    ea_vec[2] = v_fn_param;
    caml_raise_with_args(*caml_unix_exc_constr, 3, ea_vec);

    CAMLreturn0;
}

value make_flag_list(int f, int *flags, int n) {
    CAMLparam0();
    CAMLlocal2(vlist, v);
    int i;

    vlist = Val_int(0);
    for (i = 0; f && i < n; i++) {
        if (f & flags[i]) {
            v = caml_alloc(2, 0);
            Field(v, 0) = Val_int(i);
            Field(v, 1) = vlist;
            vlist = v;
            f &= ~flags[i];
        }
    }
    CAMLreturn(vlist);
}
