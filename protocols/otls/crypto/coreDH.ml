open CoreKeys

type skey = dhskey
type pkey = dhpkey

let check_element pbytes ebytes =
  true

let gen_params () : dhparams =
  {dh_p = ""; dh_g = ""}

let gen_key dh =
  let ebuf = "" in
  let skey = DHS ebuf, {dh_p = ebuf; dh_g = ebuf} in
  let pkey = DHP ebuf, {dh_p = ebuf; dh_g = ebuf} in
  skey, pkey
   
let agreement dh x y =
  ""

let save_params_to_file fname dh =
  true

let load_params_from_file fname =
  None

let load_default_params () =
  {dh_p = ""; dh_g = ""}
