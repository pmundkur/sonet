type engine

val name  : engine -> string
val digest: engine -> string -> string

val md5engine   : unit -> engine
val sha1engine  : unit -> engine
val sha256engine: unit -> engine
val sha384engine: unit -> engine
val sha512engine: unit -> engine

val md5   : string -> string
val sha1  : string -> string
val sha256: string -> string
val sha384: string -> string
val sha512: string -> string
