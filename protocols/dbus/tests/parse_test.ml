(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2009      Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public License    *)
(*  as published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or (at your option) any later version.                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

module T = Dbus_type
module V = Dbus_value

let platform_test () =
  (match Platform.get_host_endianness () with
     | Platform.Little_endian -> Printf.printf "Platform is little-endian.\n"
     | Platform.Big_endian    -> Printf.printf "Platform is big-endian.\n");
  let doubles = [3.0; 30003.3333] in
    List.iter (fun d ->
                let da = Platform.float_to_bytes d in
                let d' = Platform.float_of_bytes da in
                  assert (d = d')
             ) doubles

let serial = ref 1L

let get_serial () =
  let s = !serial in
    serial := Int64.succ !serial;
    s

let get_msgs () =
  let destination = "org.freedesktop.DBus" in
  let msgs = ref [] in
  let add_msg m = msgs := m :: !msgs
  in
    add_msg (Dbus_msglib.make_peer_ping_msg
               ~destination ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_peer_get_machine_id_msg
               ~destination ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_introspect_msg
               ~destination ~serial:(get_serial ()));

    let interface   = "org.freedesktop.DBus.test" in
    let property    = "randomTest" in
    add_msg (Dbus_msglib.make_property_get_msg
               ~destination ~serial:(get_serial ())
               ~interface ~property);

    let val_type    = T.T_base T.B_string in
    let value       = V.V_string "string" in
    add_msg (Dbus_msglib.make_property_set_msg
               ~destination ~serial:(get_serial ())
               ~interface ~property ~val_type ~value);

    let name        = "com.github.odbus.test" in

    let name_flags  = [ Dbus_msglib.Name_allow_replacement;
                        Dbus_msglib.Name_do_not_queue;
                        Dbus_msglib.Name_replace_existing ] in
    add_msg (Dbus_msglib.make_request_name_msg
               ~serial:(get_serial ()) ~name ~flags:name_flags);

    add_msg (Dbus_msglib.make_release_name_msg
               ~serial:(get_serial ()) ~name);
    add_msg (Dbus_msglib.make_hello_msg
               ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_list_names_msg
               ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_list_activatable_names_msg
               ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_name_has_owner_msg
               ~serial:(get_serial ()) ~name);
    add_msg (Dbus_msglib.make_start_service_by_name_msg
               ~serial:(get_serial ()) ~name);
    add_msg (Dbus_msglib.make_get_name_owner_msg
               ~serial:(get_serial ()) ~name);
    let connection_name = "" in
    add_msg (Dbus_msglib.make_get_connection_unix_user_msg
               ~serial:(get_serial ()) ~connection_name);
    let filter      = "type='signal',sender='org.gnome.TypingMonitor',interface='org.gnome.TypingMonitor'" in
    add_msg (Dbus_msglib.make_add_match_msg
               ~serial:(get_serial ()) ~filter);
    add_msg (Dbus_msglib.make_remove_match_msg
               ~serial:(get_serial ()) ~filter);
    add_msg (Dbus_msglib.make_get_id_msg
               ~serial:(get_serial ()));
    List.rev !msgs

let marshal_msgs verbose endian msgs =
  let _, len =
    (List.fold_left (fun (cnt, ofs) m ->
                       let mbytes = Dbus_message_marshal.compute_marshaled_size m in
                         if verbose then
                           Printf.printf "[marshal_msgs]: msg %d (at ofs=%d) has size %d\n"
                             cnt ofs mbytes;
                         cnt + 1, ofs + mbytes
                    ) (0, 0) msgs) in
  let _ = (if verbose then
             Printf.printf "[marshal_msgs] computed buf len of %d bytes for %d mesages.\n"
               len (List.length msgs)) in
  let buf = String.make len 'P' in
  let total_len = String.length buf in
  let _, final_ofs, remaining =
    List.fold_left (fun (cnt, ofs, len) m ->
                      let _ = (if verbose then
                                 Printf.printf "[marshal_msgs]: marshalling msg %d at ofs=%d\n"
                                   cnt ofs) in
                      let mbytes =
                        Dbus_message_marshal.marshal_message
                          endian buf ~offset:ofs ~length:len m in
                      let ofs, len = ofs + mbytes, len - mbytes in
                        assert (ofs <= total_len);
                        assert (len >= 0);
                        cnt + 1, ofs, len
                   ) (0, 0, total_len) msgs
  in buf, final_ofs, remaining

let test_msg_roundtrip verbose tracing endian =
  let result = ref true in
  let test_msgs = get_msgs () in
  let buf, _final_ofs, _remaining = marshal_msgs verbose endian test_msgs in
  let buflen = String.length buf in
  let rec do_parse parsed_msgs state ofs =
    match Dbus_message_parse.parse_substring state buf ofs (buflen - ofs) with
      | Dbus_message_parse.Parse_incomplete _ ->
          raise (Failure "ERROR: incomplete parse!")
      | Dbus_message_parse.Parse_result (m, remaining) ->
          if tracing then begin
            Dbus_message.pr_msg Format.std_formatter m;
            Format.pp_print_flush Format.std_formatter ();
          end;
          let parsed_msgs = m :: parsed_msgs in
          if remaining > 0 then
            do_parse parsed_msgs (Dbus_message_parse.init_state ()) (buflen - remaining)
          else
            List.rev parsed_msgs
  in
    let parsed_msgs = do_parse [] (Dbus_message_parse.init_state ()) 0 in
      List.iter2 (fun mm pm ->
                    result := !result && mm = pm;
                    if verbose then begin
                      Printf.printf "\nMarshaled msg:\n%!";
                      Dbus_message.pr_msg Format.std_formatter mm;
                      Format.pp_print_flush Format.std_formatter ();
                      Printf.printf "Parsed msg:\n%!";
                      Dbus_message.pr_msg Format.std_formatter pm;
                      Format.pp_print_flush Format.std_formatter ();
                    end else if mm <> pm then begin
                      Printf.printf "\nMarshal/Unmarshal failure:\n%!";
                      Printf.printf "Marshaled msg:\n%!";
                      Dbus_message.pr_msg Format.std_formatter mm;
                      Format.pp_print_flush Format.std_formatter ();
                      Printf.printf "Parsed msg:\n%!";
                      Dbus_message.pr_msg Format.std_formatter pm;
                      Format.pp_print_flush Format.std_formatter ();
                    end
                 ) test_msgs parsed_msgs;
      !result

let get_values () = [
  T.T_base T.B_string, V.V_string "test string";
  T.T_base T.B_uint32, V.V_uint32 (Int64.of_int (1056*256));
  (T.T_array (T.T_array (T.T_base T.B_string)),
   V.V_array [| V.V_array [| V.V_string "string11"; V.V_string "string12" |];
                V.V_array [| V.V_string "string21"; V.V_string "string22" |];
                V.V_array [| V.V_string "string31"; V.V_string "string32" |]
             |]);
  T.T_base T.B_double, V.V_double 1.0;
  T.T_base T.B_double, V.V_double 33033.0333;
]

let marshal_types endian values =
  let len =
    (List.fold_left (fun ofs (t,v) ->
                       ofs + (Dbus_type_marshal.compute_marshaled_size
                                ~offset:ofs t v)
                    ) 0 values) in
  let buf = String.make len 'P' in
  let total_len = String.length buf in
  let ctxt = (Dbus_type_marshal.init_context endian buf
                ~start_offset:0 ~offset:0 ~length:total_len) in
  let _ =
    List.fold_left (fun ctxt (t, v) ->
                      Dbus_type_marshal.marshal_complete_type ctxt t v
                   ) ctxt values
  in buf

let parse_types endian types buf =
  let ctxt = (Dbus_type_parse.init_context endian buf
                ~offset:0 ~length:(String.length buf)) in
  let _, values =
    List.fold_left (fun (ctxt, values) t ->
                      let v, ctxt = Dbus_type_parse.parse_complete_type t ctxt in
                        ctxt, v :: values
                   ) (ctxt, []) types
  in List.rev values

let test_value_roundtrip verbose _tracing endian =
  let result = ref true in
  let mvalues = get_values () in
  let types = List.map (fun (t, _) -> t) mvalues in
  let buf = marshal_types endian mvalues in
  let pvalues = parse_types endian types buf in
    List.iter2 (fun (t, mv) pv ->
                  result := !result && mv = pv;
                  if verbose then begin
                    Printf.printf "\nTest: type=%s\n" (T.to_string t);
                    Printf.printf "\t-> %s\n" (V.to_string mv);
                    Printf.printf "\t<- %s\n" (V.to_string pv);
                  end else if mv <> pv then begin
                    Printf.printf "\nFailed Test: type=%s\n" (T.to_string t);
                    Printf.printf "\t -> %s\n" (V.to_string mv);
                    Printf.printf "\t <- %s\n" (V.to_string pv);
                  end
               ) mvalues pvalues;
    !result

let print_except e bt =
  Printf.printf "Exception: %s\n" e;
  Printf.printf "%s\n" bt

let _ =
  Printexc.record_backtrace true;

  let verbose = ref false in
  let tracing = ref false in
  let larg = [
    ("-v", Arg.Set verbose, " verbose");
    ("-t", Arg.Set tracing, " tracing");
  ] in
  let usage_msg = Printf.sprintf "%s [-v] [-t]" Sys.argv.(0) in
    Arg.parse larg (fun _ -> Printf.printf "%s\n" usage_msg; exit 0) usage_msg;

    if !tracing then begin
      Dbus_message_marshal.enable_debug_log ();
      Dbus_type_marshal.enable_debug_log ();
      Dbus_message_parse.enable_debug_log ();
      Dbus_type_parse.enable_debug_log ();
    end;

    try
      platform_test ();
      if not (test_value_roundtrip !verbose !tracing T.Little_endian) then
        exit 1;
      if not (test_msg_roundtrip !verbose !tracing T.Little_endian) then
        exit 1;
    with
      | Dbus_type_marshal.Marshal_error e ->
          print_except (Dbus_type_marshal.error_message e) (Printexc.get_backtrace ())
      | Dbus_message_parse.Parse_error e ->
          print_except (Dbus_message_parse.error_message e) (Printexc.get_backtrace ())
      | Dbus_type_parse.Parse_error e ->
          print_except (Dbus_type_parse.error_message e) (Printexc.get_backtrace ())
      | e ->
          print_except (Printexc.to_string e) (Printexc.get_backtrace ())
