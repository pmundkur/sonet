type db

exception DBError of string

val opendb  : string -> db
val closedb : db -> unit
val put     : db -> string -> string -> unit
val get     : db -> string -> string option
val remove  : db -> string -> bool
val all     : db -> (string * string) list
val keys    : db -> string list
val tx      : db -> (db -> 'a) -> 'a
