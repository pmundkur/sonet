type engine
type key = string

val name: engine -> string
val mac : engine -> string -> string

val md5engine   : key -> engine
val sha1engine  : key -> engine
val sha256engine: key -> engine
val sha384engine: key -> engine
val sha512engine: key -> engine

val md5   : key -> string -> string
val sha1  : key -> string -> string
val sha256: key -> string -> string
val sha384: key -> string -> string
val sha512: key -> string -> string
