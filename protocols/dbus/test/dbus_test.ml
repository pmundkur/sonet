let system_sock_addr = "/var/run/dbus/system_bus_socket"
let system_sock_addr = "/home/prashanth/src/dbus/install-1.3.0/var/run/dbus/system_bus_socket"
let session_sock_addr = "\000/tmp/dbus-mzDgLTknAW"
let dbus_addr = session_sock_addr

let serial = ref 1L

let get_serial () =
  let s = !serial in
    serial := Int64.succ !serial;
    s

let get_hello_msg () =
  Dbus_msglib.make_hello_msg ~serial:(get_serial ())

let get_filter_msg filter =
  Dbus_msglib.make_add_match_msg ~serial:(get_serial ()) ~filter

let send conn m =
  Printf.printf "Sending: --->\n%!";
  Dbus_message.pr_msg Format.std_formatter m;
  Format.pp_print_flush Format.std_formatter ();
  Dbus_connection.send conn m

let auth_callback conn =
  Printf.printf "Authenticated!\n%!";
  (* We need to register with the bus; i.e. send a Hello message.  The
     response will contain our unique bus name.  *)
  send conn (get_hello_msg ());
  send conn (get_filter_msg "type='signal'");
  send conn (get_filter_msg "type='method_call'");
  send conn (get_filter_msg "type='method_return'");
  send conn (get_filter_msg "type='error'")

let msg_received_callback _ m =
  Printf.printf "Received: <---\n%!";
  Dbus_message.pr_msg Format.std_formatter m;
  Format.pp_print_flush Format.std_formatter ()

let shutdown_callback c =
  Printf.printf "Shutdown!\n%!";
  Dbus_connection.close c

let error_callback c (ec, m, s) =
  Printf.printf "Error in %s: %s %s!\n%!" m (Unix.error_message ec) s;
  Dbus_connection.close c

let send_done_callback _ =
  Printf.printf "Send done!\n%!"

let start_dbus_connection el dbus_addr =
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    let connected =
      try
        Unix.connect sock (Unix.ADDR_UNIX dbus_addr);
        true
      with Unix.Unix_error (Unix.EAGAIN, _, _) -> false in
    let callbacks = {
      Dbus_connection.authenticated_callback = auth_callback;
      Dbus_connection.msg_received_callback = msg_received_callback;
      Dbus_connection.shutdown_callback = shutdown_callback;
      Dbus_connection.error_callback = error_callback;
      Dbus_connection.send_done_callback = send_done_callback;
    } in
      Dbus_connection.attach el sock ~connected callbacks

let run el =
  while Eventloop.has_connections el || Eventloop.has_timers el do
    Eventloop.dispatch el 1.0
  done

let main dbus_addr =
  let el = Eventloop.create () in
  let _ = start_dbus_connection el dbus_addr
  in run el

let print_except e bt =
  Printf.printf "Exception: %s\n" e;
  Printf.printf "%s\n" bt

let _ =
  Printexc.record_backtrace true;

  let verbose = ref false in
  let trace_input = ref false in
  let show_input = ref false in
  let trace_output = ref false in
  let dbus_addr = ref "" in
  let larg = [
    ("-v",  Arg.Set verbose,      " verbose");
    ("-it", Arg.Set trace_input,  " input tracing");
    ("-ixt", Arg.Set show_input,  " input data tracing");
    ("-ot", Arg.Set trace_output, " output tracing");
  ] in
  let usage_msg = Printf.sprintf "%s [-v] [-it] [-ixt] [-ot]" Sys.argv.(0) in
    Arg.parse larg (fun s -> dbus_addr := s) usage_msg;

    if !trace_input then begin
      Dbus_message_parse.enable_debug_log ();
      Dbus_type_parse.enable_debug_log ();
    end;
    if !trace_output then begin
      Dbus_message_marshal.enable_debug_log ();
      Dbus_type_marshal.enable_debug_log ();
    end;
    if !show_input then begin
      Dbus_message_parse.enable_debug_log ();
      Dbus_message_parse.enable_data_trace ();
    end;

    try
      main (if !dbus_addr = ""
            then system_sock_addr
            else "\000" ^ !dbus_addr)
    with
      | Unix.Unix_error (ec, m, s) as ex ->
          Printf.printf "Unix error: %s (%s %s)\n" (Unix.error_message ec) m s;
          print_except (Printexc.to_string ex) (Printexc.get_backtrace ())
      | Dbus_message_parse.Parse_error e as ex ->
          Printf.printf "Message parse error: %s\n" (Dbus_message_parse.error_message e);
          print_except (Printexc.to_string ex) (Printexc.get_backtrace ())
      | Dbus_type_parse.Parse_error e as ex ->
          Printf.printf "Message parse error: %s\n" (Dbus_type_parse.error_message e);
          print_except (Printexc.to_string ex) (Printexc.get_backtrace ())
      | ex ->
          print_except (Printexc.to_string ex) (Printexc.get_backtrace ())
