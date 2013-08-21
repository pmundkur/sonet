open Capsicum

let test_is_sandboxed () =
  Printf.printf "sandbox is %s\n" (if is_sandboxed () then "on" else "off")

let run_tests () =
  Eventloop.init ();
  test_is_sandboxed ()

let () =
  try run_tests ()
  with Unix.Unix_error (ec, fn, em) ->
    Printf.eprintf "unix error: %s: %s\n" fn (Unix.error_message ec)
