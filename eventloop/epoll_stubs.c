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

#include <errno.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include "eventloop.h"
#include "posix_stubs.h"

#ifndef linux

CAMLprim value stub_epoll_create(value size) {
    CAMLparam1(size);
    raise_unix_error(ENOTSUP, "epoll_create", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_add(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_remove(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_enable_recv(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_disable_recv(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_enable_send(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_disable_send(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll_ctl", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_is_recv_enabled(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_is_send_enabled(value es, value fd) {
    CAMLparam2(es, fd);
    raise_unix_error(ENOTSUP, "epoll", "");
    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_get_events(value es, value timeout) {
    CAMLparam2(es, timeout);
    raise_unix_error(ENOTSUP, "epoll", "");
    CAMLreturn(Val_unit);
}

#else

#include <sys/epoll.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#ifndef MAX_EPOLL_FDS
#define MAX_EPOLL_FDS 2048
#endif

#ifndef MAX_EPOLL_WAIT_EVENTS
#define MAX_EPOLL_WAIT_EVENTS 32
#endif

struct epoll_state {
    int epoll_fd;
    int watch_array_size;
    uint32_t watch_array[1];
};

#define Epoll_state_val(v) (*((struct epoll_state **) Data_custom_val(v)))

static void epoll_state_finalize(value v) {
    struct epoll_state *eps;

    eps = Epoll_state_val(v);

    if (eps->epoll_fd > 0) close(eps->epoll_fd);

    free(eps);
}

static struct custom_operations epoll_state_ops = {
    "http://github.com/pmundkur/onet",
    epoll_state_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

CAMLprim value stub_epoll_create(value size) {
    CAMLparam1(size);
    CAMLlocal1(result);

    int epoll_fd, nfds, eps_size;
    struct epoll_state *eps;
    struct rlimit rlim;

    epoll_fd = epoll_create(10);
    if (epoll_fd < 0)
        raise_unix_error(errno, "epoll_create", "");
    nfds = getrlimit(RLIMIT_NOFILE, &rlim);
    if (nfds < 0)
        raise_unix_error(errno, "getrlimit", "RLIMIT_NOFILE");
    if (RLIM_INFINITY == rlim.rlim_cur)
        nfds = (Int_val(size) != 0) ? Int_val(size) : MAX_EPOLL_FDS;
    else
        nfds = (int) rlim.rlim_cur;
    eps_size = sizeof(struct epoll_state) + (nfds - 1)*sizeof(uint32_t);
    eps = (struct epoll_state *) malloc(eps_size);
    if (NULL == eps)
        raise_unix_error(ENOMEM, "malloc", "stub_epoll_create");
    eps->epoll_fd = epoll_fd;
    eps->watch_array_size = nfds;
    memset(eps->watch_array, 0, nfds * sizeof(uint32_t));

    /* Since in normal usage, epoll_state values are expected to be long-lived,
       we should not have too many of these allocated at any time (in fact,
       normally, just one), it makes sense to collect soon after finalization.
       Hence we use a used/max ratio of 1/2.
    */
    result = caml_alloc_custom(&epoll_state_ops,
                               eps_size,
                               1,
                               2);
    Epoll_state_val(result) = eps;
    CAMLreturn(result);
}

CAMLprim value stub_epoll_add(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_add", "");

    /* Set EPOLLRDHUP so that we detect closed connections even if no watches
       are ever enabled. */
    event.events = c_es->watch_array[Int_val(fd)] = EPOLLRDHUP;
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_ADD, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "EPOLL_CTL_ADD");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_remove(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_remove", "");

    event.events = c_es->watch_array[Int_val(fd)] = 0;
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_DEL, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "EPOLL_CTL_DEL");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_enable_recv(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_enable_recv", "");

    c_es->watch_array[Int_val(fd)] |= EPOLLIN;
    event.events = c_es->watch_array[Int_val(fd)];
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_MOD, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "enable_recv");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_disable_recv(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_disable_recv", "");

    c_es->watch_array[Int_val(fd)] &= ~EPOLLIN;
    event.events = c_es->watch_array[Int_val(fd)];
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_MOD, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "disable_recv");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_enable_send(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_enable_send", "");

    c_es->watch_array[Int_val(fd)] |= EPOLLOUT;
    event.events = c_es->watch_array[Int_val(fd)];
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_MOD, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "enable_send");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_disable_send(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;
    struct epoll_event event;

    memset(&event, 0, sizeof(event));
    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_disable_send", "");

    c_es->watch_array[Int_val(fd)] &= ~EPOLLOUT;
    event.events = c_es->watch_array[Int_val(fd)];
    event.data.fd = Int_val(fd);
    if (epoll_ctl(c_es->epoll_fd, EPOLL_CTL_MOD, Int_val(fd), &event) < 0)
        raise_unix_error(errno, "epoll_ctl", "disable_send");

    CAMLreturn(Val_unit);
}

CAMLprim value stub_epoll_is_recv_enabled(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;

    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_is_recv_enabled", "");

    CAMLreturn(Val_bool(c_es->watch_array[Int_val(fd)] & EPOLLIN));
}

CAMLprim value stub_epoll_is_send_enabled(value es, value fd) {
    CAMLparam2(es, fd);

    struct epoll_state *c_es;

    c_es = Epoll_state_val(es);

    if (Int_val(fd) > c_es->watch_array_size)
        raise_unix_error(EINVAL, "stub_epoll_is_send_enabled", "");

    CAMLreturn(Val_bool(c_es->watch_array[Int_val(fd)] & EPOLLOUT));
}

void fill_event(value result, int idx, int fd, int ev_constr) {
    CAMLparam1(result);
    CAMLlocal1(entry);

    /* The value needs to correspond to:
       { event_type = ev_constr; event_fd = fd }
     */
    entry = caml_alloc_small(2, 0);
    Field(entry, RECORD_EVENT_TYPE_OFS) = Val_int(ev_constr);
    Field(entry, RECORD_EVENT_FD_OFS) = Val_int(fd);

    Store_field(result, idx, entry);

    CAMLreturn0;
}

value prepare_get_events_result(struct epoll_event *events, int n_fdevents) {
    CAMLparam0();
    CAMLlocal1(result);

    int i, idx, n_oevents;

    /* Compute the number of OCaml events to return, which may be more than
       n_fdevents if multiple events occur for a single fd.
    */
    n_oevents = 0;
    for (i = 0; i < n_fdevents; i++) {
        if (events[i].events & (EPOLLIN | EPOLLRDHUP))
            n_oevents++;
        if (events[i].events & EPOLLOUT)
            n_oevents++;
        if (events[i].events & (EPOLLERR | EPOLLHUP))
            n_oevents++;
    }

    result = caml_alloc(n_oevents, 0);

    for (i = 0, idx = 0; i < n_fdevents; i++) {
        if (events[i].events & (EPOLLIN | EPOLLRDHUP))
            fill_event(result, idx++, events[i].data.fd, CONSTR_EV_READABLE);
        if (events[i].events & EPOLLOUT)
            fill_event(result, idx++, events[i].data.fd, CONSTR_EV_WRITEABLE);
        if (events[i].events & (EPOLLERR | EPOLLHUP))
            fill_event(result, idx++, events[i].data.fd, CONSTR_EV_PENDING_ERROR);
    }

    CAMLreturn(result);
}

CAMLprim value stub_epoll_get_events(value es, value timeout) {
    CAMLparam2(es, timeout);
    CAMLlocal1(result);

    int n_fdevents, c_timeout, epoll_fd;
    struct epoll_event events[MAX_EPOLL_WAIT_EVENTS];

    c_timeout = (int) 1000.0 * (Double_val(timeout) > 0
                                ? (Double_val(timeout) + 0.5)
                                : (Double_val(timeout) - 0.5));
    epoll_fd = Epoll_state_val(es)->epoll_fd;

    caml_enter_blocking_section();
    n_fdevents = epoll_wait(epoll_fd, events, MAX_EPOLL_WAIT_EVENTS, c_timeout);
    caml_leave_blocking_section();

    if (n_fdevents < 0)
        raise_unix_error(errno, "epoll_wait", "");

    result = prepare_get_events_result(events, n_fdevents);

    CAMLreturn(result);
}

CAMLprim value stub_test_get_events(value es) {
    CAMLparam1(es);
    CAMLlocal1(result);

    int n_fdevents;
    struct epoll_event events[5];

    n_fdevents = sizeof(events)/sizeof(struct epoll_event);

    events[0].events = EPOLLIN | EPOLLOUT;
    events[0].data.fd = 1;

    events[1].events = EPOLLERR;
    events[1].data.fd = 2;

    events[2].events = EPOLLERR;
    events[2].data.fd = 1;

    events[3].events = EPOLLRDHUP;
    events[3].data.fd = 3;

    events[4].events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLHUP;
    events[4].data.fd = 4;

    result = prepare_get_events_result(events, n_fdevents);

    CAMLreturn(result);
}

#endif
