open Bytes

type pk = {pk : CoreACiphers.pk}
type sk = {sk : CoreACiphers.sk}

type modulus  = CoreKeys.modulus
type exponent = CoreKeys.exponent

let create_rsaskey ((m, e) : modulus * exponent) = {sk = CoreACiphers.RSASKey(m, e)}
let create_rsapkey ((m, e) : modulus * exponent) = {pk = CoreACiphers.RSAPKey(m, e)}

let repr_of_rsapkey ({pk = pk}) = pk
let repr_of_rsaskey ({sk = sk}) = sk
