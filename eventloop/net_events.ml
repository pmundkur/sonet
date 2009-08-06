type event_type =
  | Readable
  | Writeable
  | PendingError
  | Error of Unix.error * string * string
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

let remove_events fd ?(start_indx=0) events =
  for indx = start_indx to (Array.length events) - 1 do
    let e = events.(indx) in
      if e.event_fd = fd
      then events.(indx) <- { event_type = Removed; event_fd = fd }
  done
