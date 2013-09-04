type db = unit

exception DBError of string

let opendb name =
  ()

let closedb db =
  ()

let put db key value =
  ()
let get db key =
  None

let remove db key =
  false

let all db =
  []

let keys db =
  []

let tx db f =
  f db

  
