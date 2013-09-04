type key = Key of string
type iv  = IV of string

val aes_cbc_encrypt: key -> iv -> string -> string
val aes_cbc_decrypt: key -> iv -> string -> string

val des3_cbc_encrypt: key -> iv -> string -> string
val des3_cbc_decrypt: key -> iv -> string -> string

type rc4engine

val rc4create : key -> rc4engine
val rc4process: rc4engine -> string -> string
