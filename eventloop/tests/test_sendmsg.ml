open Sendmsg

let sflags = []

let show_cred c =
  Printf.sprintf "pid:%d uid:%d gid:%d" c.pid c.uid c.gid

let show_cmsg = function
  | Cmsg_generic (l, t, s) ->
    Printf.printf "generic (%d, %d): %s\n" l t s
  | Cmsg_scm_rights l ->
    Printf.printf "rights: %d fds\n" (List.length l)
  | Cmsg_scm_credentials c ->
    Printf.printf "creds: %s\n" (show_cred c)

let make_msg () =
  let iov = ["a"; "b"; "c"] in
  let cmsgs = [(Cmsg_scm_rights [Unix.stdin; Unix.stdout; Unix.stderr]);
               (Cmsg_scm_rights [Unix.stdin; Unix.stdout]);
               (Cmsg_scm_credentials (getcred ()))
              ] in
  let rflags = [] in
  {msg_iovec = iov; msg_cmsgs = cmsgs; msg_flags = rflags}

let show_msg m =
  Printf.printf "msg: %d iovecs, %d cmsgs, %d flags\n%!"
    (List.length m.msg_iovec) (List.length m.msg_cmsgs) (List.length m.msg_flags);
  List.iter (fun s -> Printf.printf " iov(%d): %s\n" (String.length s) s) m.msg_iovec;
  List.iter (fun f -> Printf.printf " flag: %s\n" (msg_flag_name f)) m.msg_flags;
  List.iter (fun c -> show_cmsg c) m.msg_cmsgs

let make_socks () =
  Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0

let test_sendrecv () =
  let sndr, rcvr = make_socks () in
  let smsg = make_msg () in
  set_recvcred rcvr true;
  Printf.printf "test_send: %d\n%!" (Sendmsg.sendmsg sndr smsg sflags);
  show_msg (Sendmsg.recvmsg rcvr [])

let run_tests () =
  Eventloop.init ();
  test_sendrecv ()

let () =
  try run_tests ()
  with Unix.Unix_error (ec, fn, em) ->
    Printf.eprintf "unix error: %s: %s\n" fn (Unix.error_message ec)
