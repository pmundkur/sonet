type modulus  = Modulus of string
type exponent = Exponent of string

type rsapkey = modulus * exponent
type rsaskey = modulus * exponent

type dsaparams = {dsa_p : string; dsa_q : string; dsa_g : string}

type dsapkey = DSP of string * dsaparams
type dsaskey = DSS of string * dsaparams

type dhparams = {dh_p : string; dh_g : string}

type dhpbytes = DHP of string
type dhsbytes = DHS of string

type dhpkey = dhpbytes * dhparams
type dhskey = dhsbytes * dhparams
