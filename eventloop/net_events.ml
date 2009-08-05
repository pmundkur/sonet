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

module type Event_poller = sig
  type t

  val create : unit -> t

  val add : t -> Unix.file_descr -> unit
  val remove : t -> Unix.file_descr -> unit

  val enable_recv : t -> Unix.file_descr -> unit
  val disable_recv : t -> Unix.file_descr -> unit
  val enable_send : t -> Unix.file_descr -> unit
  val disable_send : t -> Unix.file_descr -> unit

  val is_recv_enabled : t -> Unix.file_descr -> bool
  val is_send_enabled : t -> Unix.file_descr -> bool

  val get_events : t -> float -> event array
end

let remove_events fd ?(start_indx=0) events =
  for indx = start_indx to (Array.length events) - 1 do
    let e = events.(indx) in
      if e.event_fd = fd
      then events.(indx) <- { event_type = Removed; event_fd = fd }
  done
