type event_type =
  | Readable
  | Writeable
  | PendingError
  | Error of Unix.error * string * string

  (* This is a special value that is never returned by
     get_events, but inserted by the caller using
     remove_events.
  *)
  | Removed

type event = {
  event_type : event_type;
  event_fd : Unix.file_descr;
}

type poller = {
  add : Unix.file_descr -> unit;
  remove : Unix.file_descr -> unit;

  enable_recv : Unix.file_descr -> unit;
  disable_recv : Unix.file_descr -> unit;

  enable_send : Unix.file_descr -> unit;
  disable_send : Unix.file_descr -> unit;

  is_recv_enabled : Unix.file_descr -> bool;
  is_send_enabled : Unix.file_descr -> bool;

  get_events : float -> event array;
}

(* (remove_events fd ~start_indx events) marks any event for fd as
   removed in the events array, starting from the specified index.

   This is usually used when while events retrieved from
   Event_poller.get_events are being dispatched, a call is made to
   close or remove a fd.
*)
val remove_events : Unix.file_descr -> ?start_indx:int -> event array -> unit
