type skey = CoreKeys.dhskey
type pkey = CoreKeys.dhpkey

val check_element: string -> string -> bool
val gen_params: unit -> CoreKeys.dhparams
val gen_key: CoreKeys.dhparams -> skey * pkey
val agreement: CoreKeys.dhparams -> CoreKeys.dhsbytes -> CoreKeys.dhpbytes -> string

val save_params_to_file: string -> CoreKeys.dhparams -> bool
val load_params_from_file: string -> CoreKeys.dhparams option
val load_default_params: unit -> CoreKeys.dhparams
