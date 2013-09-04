open Error

type hint = string
type cert = string

type chain = cert list
type sign_cert = (chain * Sig.alg * Sig.skey) option
type enc_cert = (chain * RSAKey.sk) option

val for_signing : Sig.alg list -> hint -> Sig.alg list -> sign_cert
val for_key_encryption : Sig.alg list -> hint -> enc_cert

val get_public_signing_key : cert -> Sig.alg -> Sig.pkey result
val get_public_encryption_key : cert -> RSAKey.pk result

val is_for_signing : cert -> bool
val is_for_key_encryption : cert -> bool

val get_chain_public_signing_key : chain -> Sig.alg -> Sig.pkey result
val get_chain_public_encryption_key : chain -> RSAKey.pk result

val is_chain_for_signing : chain -> bool
val is_chain_for_key_encryption : chain -> bool

val get_chain_key_algorithm : chain -> TLSConstants.sigAlg option

val get_hint : chain -> hint option

val validate_cert_chain : Sig.alg list -> chain -> bool

val certificateListBytes: chain -> string
val parseCertificateList: string -> chain -> chain result
