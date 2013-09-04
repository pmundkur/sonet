type sk
type pk

type modulus
type exponent

val create_rsapkey: modulus * exponent -> pk
val create_rsaskey: modulus * exponent -> sk

val repr_of_rsapkey: pk -> CoreACiphers.pk
val repr_of_rsaskey: sk -> CoreACiphers.sk
