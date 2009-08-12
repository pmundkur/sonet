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

let serial = ref 0L

let get_serial () =
  let s = !serial in
    serial := Int64.succ !serial;
    s

let get_msgs () =
  let destination = "org.freedesktop.DBus" in
  let msgs = ref [] in
  let add_msg m = msgs := m :: !msgs
  in
    add_msg (Dbus_msglib.make_peer_get_machine_id_msg
	       ~destination ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_introspect_msg
	       ~destination ~serial:(get_serial ()));
    add_msg (Dbus_msglib.make_peer_ping_msg
	       ~destination ~serial:(get_serial ()));
    !msgs

let marshal_msgs endian msgs =
  let len =
    (List.fold_left (fun ofs m ->
		       let next = Dbus_message_marshal.compute_marshaled_size ofs m
		       in Printf.printf "Computed marshaled size of %d bytes ..\n" next;
			 ofs + next
		    ) 0 msgs) in
  let buf = String.make (len + 1) 'P' in
  let total_len = String.length buf in
  let final_ofs, remaining =
    List.fold_left (fun (ofs, len) m ->
		      let mbytes =
			Dbus_message_marshal.marshal_message ~stream_offset:ofs
			  endian buf ~offset:ofs ~length:len m in
		      let ofs, len = ofs + mbytes, len - mbytes in
			Printf.printf "Marshaled %d bytes ...\n" mbytes;
			assert (ofs <= total_len);
			assert (len >= 0);
			ofs, len
		   ) (0, total_len) msgs
  in buf, final_ofs

let test endian =
  let buf, final_ofs = marshal_msgs endian (get_msgs ()) in
    Printf.printf "Marshalled %d bytes into a buf of length %d.\n"
      final_ofs (String.length buf)

let _ =
  test Dbus_type.Little_endian
