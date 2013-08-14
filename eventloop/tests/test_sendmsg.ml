open Sendmsg

let sflags = [SEND_DONTWAIT]

let make_msg () =
  let iov = ["a"; "b"; "c"] in
  let cmsgs = [Cmsg_generic (0,0,"cmsg0\000");
               Cmsg_generic (1,1,"cmsg1\000");] in
  let rflags = [RECV_ERRQUEUE] in
  {msg_iovec = iov; msg_cmsgs = cmsgs; msg_flags = rflags}

let make_socks () =
  Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0

let test_send () =
  let sock = fst (make_socks ()) in
  let msg = make_msg () in
  Sendmsg.sendmsg sock msg sflags

let run_tests () =
  Eventloop.init ();
  Printf.printf "test_send: %d\n" (test_send ())

let () =
  try run_tests ()
  with Unix.Unix_error (ec, fn, em) ->
    Printf.eprintf "unix error: %s: %s\n" fn (Unix.error_message ec)
