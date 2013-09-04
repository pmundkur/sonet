type sk = RSASKey of CoreKeys.rsaskey
type pk = RSAPKey of CoreKeys.rsapkey

type plain = Plain of string
type ctxt  = Ctxt of string

let encrypt_pkcs1 pk plain =
  Ctxt ""

let decrypt_pkcs1 sk ctxt =
  None
