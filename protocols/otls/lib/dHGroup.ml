open Bytes

type p   = bytes
type elt = bytes
type g   = elt

let dhparams p g : CoreKeys.dhparams =
  {CoreKeys.dh_p = p; dh_g = g}

let genElement p g : elt =
    let (_, (CoreKeys.DHP e, _)) = CoreDH.gen_key (dhparams p g) in
    e

let checkElement (p:p) (b:bytes) : elt option =
    if CoreDH.check_element p b
    then Some b
    else None
