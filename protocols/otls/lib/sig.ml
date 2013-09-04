open Bytes
open TLSConstants

type alg = sigHashAlg

type text = bytes
type sigv = bytes

type skey = {skey : CoreSig.sigskey * hashAlg}
type pkey = {pkey : CoreSig.sigpkey * hashAlg}

let create_skey (h : hashAlg) (p : CoreSig.sigskey) = {skey = (p, h)}
let create_pkey (h : hashAlg) (p : CoreSig.sigpkey) = {pkey = (p, h)}

let repr_of_skey {skey = skey} = skey
let repr_of_pkey {pkey = pkey} = pkey

let sigalg_of_skeyparams = function
  | CoreSig.SK_RSA _ -> SA_RSA
  | CoreSig.SK_DSA _ -> SA_DSA

let sigalg_of_pkeyparams = function
  | CoreSig.PK_RSA _ -> SA_RSA
  | CoreSig.PK_DSA _ -> SA_DSA

let sign (a: alg) (sk: skey) (t: text): sigv =
  let asig, ahash = a in
  let {skey = (kparams, khash)} = sk in

  if ahash <> khash then
    Error.unexpectedError
      (Printf.sprintf "Sig.sign: requested sig-hash = , but key requires "
         (*ahash khash*));
  if asig <> sigalg_of_skeyparams kparams then
    Error.unexpectedError
      (Printf.sprintf "Sig.sign: requested sig-algo = , but key requires "
         (*asig (sigalg_of_skeyparams kparams)*));

  let signature =
    match khash with
    | NULL    -> CoreSig.sign None                     kparams t
    | MD5     -> CoreSig.sign (Some CoreSig.SH_MD5)    kparams t
    | SHA     -> CoreSig.sign (Some CoreSig.SH_SHA1  ) kparams t
    | SHA256  -> CoreSig.sign (Some CoreSig.SH_SHA256) kparams t
    | SHA384  -> CoreSig.sign (Some CoreSig.SH_SHA384) kparams t
    | MD5SHA1 ->
      let t = Hash.hash MD5SHA1 t in
      CoreSig.sign None kparams t
  in signature

let verify (a : alg) (pk : pkey) (t : text) (s : sigv) =
  let asig, ahash = a in
  let {pkey = (kparams, khash)} = pk in

  if ahash <> khash then
    Error.unexpectedError
      (Printf.sprintf "Sig.verify: requested sig-hash = , but key requires "
         (*ahash khash*));
  if asig <> sigalg_of_pkeyparams kparams then
    Error.unexpectedError
      (Printf.sprintf "Sig.verify: requested sig-algo = , but key requires "
         (*asig (sigalg_of_pkeyparams kparams)*));

  let result =
    match khash with
    | NULL    -> CoreSig.verify None                     kparams t s
    | MD5     -> CoreSig.verify (Some CoreSig.SH_MD5)    kparams t s
    | SHA     -> CoreSig.verify (Some CoreSig.SH_SHA1  ) kparams t s
    | SHA256  -> CoreSig.verify (Some CoreSig.SH_SHA256) kparams t s
    | SHA384  -> CoreSig.verify (Some CoreSig.SH_SHA384) kparams t s
    | MD5SHA1 ->
      let t = Hash.hash MD5SHA1 t in
      CoreSig.verify None kparams t s
  in result

let gen (a:alg) : pkey * skey =
  let asig, ahash  = a in
  let (pkey, skey) =
    match asig with
    | SA_RSA -> CoreSig.gen CoreSig.SA_RSA
    | SA_DSA -> CoreSig.gen CoreSig.SA_DSA
    | _      -> Error.unexpectedError "[gen] invoked on unsupported algorithm"
  in
  {pkey = (pkey, ahash)}, {skey = (skey, ahash)}

let leak (a:alg) (s:skey) : CoreSig.sigskey =
  let (sk, ahash) = repr_of_skey s
  in sk

let coerce (a:alg)  (p:pkey) (csk:CoreSig.sigskey) : skey =
  let (_, ahash) = a in
  create_skey ahash csk
