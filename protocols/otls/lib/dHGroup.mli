open Bytes

type p = bytes
type elt = bytes
type g = elt

val genElement: p -> g -> elt
val checkElement: p -> bytes -> elt option
