module type Poller = sig
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

val get_events : t -> float -> Net_events.event array
end
