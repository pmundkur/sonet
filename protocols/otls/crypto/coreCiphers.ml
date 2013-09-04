type key = Key of string
type iv  = IV of string

let aes_cbc_encrypt key iv buf =
  buf

let aes_cbc_decrypt key iv buf =
  buf

let des3_cbc_encrypt key iv buf =
  buf

let des3_cbc_decrypt key iv buf =
  buf

type rc4engine = unit

let rc4create key =
  ()

let rc4process engine buf =
  buf
