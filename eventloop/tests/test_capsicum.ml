open Capsicum

let test_is_sandboxed () =
  Printf.printf "sandbox is %s\n" (if is_sandboxed () then "on" else "off")

let () =
  test_is_sandboxed ()
