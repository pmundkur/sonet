open Bytes
open TLSConstants

(* Parametric hash algorithm (implements interface) *)
let hash alg data =
  match alg with
  | NULL    -> data
  | MD5SHA1 -> CoreHash.md5 data @| CoreHash.sha1 data
  | MD5     -> CoreHash.md5    data
  | SHA     -> CoreHash.sha1   data
  | SHA256  -> CoreHash.sha256 data
  | SHA384  -> CoreHash.sha384 data
