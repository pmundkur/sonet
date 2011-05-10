#ifndef _EVENTLOOP_H
#define _EVENTLOOP_H

/*  IMPORTANT: Keep these #defines in sync with the definitions in
 *  net_events.mli and the exception name in eventloop.ml.
 */

#define CONSTR_EV_READABLE          0
#define CONSTR_EV_WRITEABLE         1
#define CONSTR_EV_PENDING_ERROR     2
#define CONSTR_EV_REMOVED           3

#define CONSTR_EV_ERROR             0

#define RECORD_EVENT_TYPE_OFS       0
#define RECORD_EVENT_FD_OFS         1

#define UNIX_EXCEPTION_NAME         "sonet.unix_error_exception"

#endif
