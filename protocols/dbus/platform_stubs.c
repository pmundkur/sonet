/*
 * Copyright (C) 2009      Prashanth Mundkur
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

#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/signals.h>

/*  IMPORTANT: Keep these #defines in sync with the definitions in
 *  platform.mli.
 */

#define CONSTR_LITTLE_ENDIAN    0
#define CONSTR_BIG_ENDIAN       1
#define UNKNOWN_ENDIAN          2

#define UNKNOWN_ENDIAN_EXCEPTION_NAME   "onet.unknown_endian_exception"

int get_host_endianness(void) {
    uint64_t u64;
    unsigned const char *u;

    u64 = 0x0001020304050607LL;
    u = (unsigned const char *) &u64;

    if (0x07 == u[0] && 0x06 == u[1] && 0x05 == u[2] && 0x04 == u[3]
        && 0x03 == u[4] && 0x02 == u[5] && 0x01 == u[6] && 0x00 == u[7])
        return CONSTR_LITTLE_ENDIAN;
    else if (0x00 == u[0] && 0x01 == u[1] && 0x02 == u[2] && 0x03 == u[3]
             && 0x04 == u[4] && 0x05 == u[5] && 0x06 == u[6] && 0x07 == u[7])
        return CONSTR_BIG_ENDIAN;
    return UNKNOWN_ENDIAN;
}

CAMLprim value stub_get_host_endianness(value unit) {
    CAMLparam1(unit);

    static value *caml_unknown_endian_exc_constr;
    if (NULL == caml_unknown_endian_exc_constr) {
        caml_unknown_endian_exc_constr = caml_named_value(UNKNOWN_ENDIAN_EXCEPTION_NAME);
        if (NULL == caml_unknown_endian_exc_constr)
            invalid_argument("Exception Unknown_endian is not initialized!");
    }

    int endian = get_host_endianness();
    if (UNKNOWN_ENDIAN == endian)
        caml_raise_constant(*caml_unknown_endian_exc_constr);

    CAMLreturn(Val_int(endian));
}

CAMLprim value stub_float_to_bytes (value dub) {
    CAMLparam1(dub);
    CAMLlocal1(result);

    double d;
    unsigned const char *c;
    int i;

    d = Double_val(dub);
    result = caml_alloc_small(8, 0);
    for (i = 0, c = (unsigned const char *)&d; i < 8; i++, c++)
        Field(result, i) = Val_int(*c);

    CAMLreturn(result);
}

CAMLprim value stub_float_of_bytes (value bytes) {
    CAMLparam1(bytes);

    double d;
    unsigned char *c;
    int i;

    for (i = 0, c = (unsigned char *)&d; i < 8; i++, c++)
        *c = Int_val(Field(bytes, i));

    CAMLreturn(caml_copy_double(d));
}

#ifdef _UNIT_TEST_
#include <stdio.h>

int main(int argc, const char *argv[]) {
    switch (get_host_endianness()) {
    case CONSTR_LITTLE_ENDIAN:
        printf("platform is little-endian.\n");
        break;
    case CONSTR_BIG_ENDIAN:
        printf("platform is big-endian.\n");
        break;
    default:
        printf("platform has unknown-endianess.\n");
    }
    return 0;
}
#endif
