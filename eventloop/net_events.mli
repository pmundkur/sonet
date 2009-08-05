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

(* (remove_events fd ~start_indx events) marks any event for fd as
   removed in the events array, starting from the specified index.

   This is usually used when while events retrieved from
   Event_poller.get_events are being dispatched, a call is made to
   close or remove a fd.
*)
val remove_events : Unix.file_descr -> ?start_indx:int -> event array -> unit
