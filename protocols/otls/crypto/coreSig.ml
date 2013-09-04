open CoreKeys

type sighash =
| SH_MD5
| SH_SHA1
| SH_SHA256
| SH_SHA384

type sigalg =
| SA_RSA
| SA_DSA

type sigskey =
| SK_RSA of CoreKeys.rsaskey
| SK_DSA of CoreKeys.dsaskey

type sigpkey =
| PK_RSA of CoreKeys.rsapkey
| PK_DSA of CoreKeys.dsapkey

type text = string
type sigv = string

let sigalg_of_skey = function
  | SK_RSA _ -> SA_RSA
  | SK_DSA _ -> SA_DSA

let sigalg_of_pkey = function
  | PK_RSA _ -> SA_RSA
  | PK_DSA _ -> SA_DSA

let gen (a : sigalg) : sigpkey * sigskey =
  let ebuf = "" in
  let dsap = {dsa_p = ebuf; dsa_q = ebuf; dsa_g = ebuf} in
    match a with
    | SA_RSA ->
      PK_RSA (Modulus ebuf, Exponent ebuf),
      SK_RSA (Modulus ebuf, Exponent ebuf)
    | SA_DSA ->
      PK_DSA (DSP (ebuf, dsap)), SK_DSA (DSS (ebuf, dsap))

let sign (ahash : sighash option) (sk : sigskey) (t : text) : sigv =
    match sk with
    | SK_RSA sk -> ""
    | SK_DSA sk -> ""

let verify (ahash : sighash option) (pk : sigpkey) (t : text) (s : sigv) =
  false
