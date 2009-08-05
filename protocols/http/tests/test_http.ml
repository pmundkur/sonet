(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Http

let do_requests = ref false
let do_responses = ref false
let test_compact_api = ref false
let show_headers = ref false
let show_payload = ref false

let parse_args () =
	let options = [("-rq", Arg.Set do_requests, " parse request stream");
		       ("-rp", Arg.Set do_responses, " parse response stream");
		       ("-c", Arg.Set test_compact_api, " test compact api");
		       ("-ph", Arg.Set show_headers, " show headers");
		       ("-pp", Arg.Set show_payload, " show payload (only for non-compact!)")
		      ] in
	let file = ref None in
	let usage = Printf.sprintf "Usage: %s [options] file" Sys.argv.(0) in
	Arg.parse (Arg.align options) (fun f -> file := Some f) usage;
	match !file, !do_requests, !do_responses with
	| None, _, _ ->
		Arg.usage (Arg.align options) usage; exit 1
	| _, false, false ->
		Printf.eprintf "Please specify whether the input contains HTTP requests or responses.\n";
		Arg.usage (Arg.align options) usage; exit 1
	| _, true, true ->
		Printf.eprintf "Please specify only one of -requests or -responses.\n";
		Arg.usage (Arg.align options) usage; exit 1
	| Some f, _, _ -> f

let read_whole_file ic =
	let buf = Buffer.create 512 in
	try
		let rec do_read () =
			Buffer.add_char buf (input_char ic);
			do_read ()
		in do_read ()
	with End_of_file ->
		Buffer.contents buf

let print_request req =
	if !show_headers then begin
		let buf = Buffer.create 512 in
		Request_header.serialize buf req;
		Printf.printf "%s" (Buffer.contents buf)
	end else
		Printf.printf "%s %s\n"
			(Request_header.string_of_meth req.Request_header.meth)
			req.Request_header.uri

let print_response resp =
	if !show_headers then begin
		let buf = Buffer.create 512 in
		Response_header.serialize buf resp;
		Printf.printf "%s" (Buffer.contents buf)
	end else
		Printf.printf "%d %s\n"
			resp.Response_header.status_code
			resp.Response_header.reason_phrase

let print_payload serializer p =
	if !show_payload then
		let buf = Buffer.create 512 in
		serializer buf p;
		Printf.printf "%s" (Buffer.contents buf)
	else
		Printf.printf " Payload: %d bytes.\n"
			(Buffer.length p.Payload.content)

let parse_requests inp =
	let input = ref inp in
	let state = ref (Request_header.init_state ()) in
	while String.length !input > 0 do
		match Request_header.parse !state !input with
		| Request_header.Result (r, consumed) ->
			print_request r;
			input := String.sub !input consumed ((String.length !input) - consumed);
			(* Check for payload *)
			(match Payload.init_from_request r with
			 | Payload.No_payload ->
				Printf.printf " No payload.\n"
			 | Payload.Error s ->
				Printf.printf " Error: %s\n" s; exit 1
			 | Payload.Payload state ->
				(match Payload.parse state !input with
				 | Payload.Result (p, consumed) ->
					print_payload (Payload.serialize_of_request r) p;
					input := String.sub !input consumed ((String.length !input) - consumed);
				 | Payload.Parse_incomplete s ->
					(* We might have to simulate a Connection close. *)
					Payload.connection_closed s;
					(match Payload.get_parse_result s with
					 | None ->
						Printf.printf " Payload incomplete!\n"
					 | Some p ->
						print_payload (Payload.serialize_of_request r) p
					);
					input := ""
				)
			);
			state := Request_header.init_state ()
		| Request_header.Parse_incomplete st ->
			if Request_header.num_bytes_parsed st > 0 then
				Printf.printf "Request incomplete!\n";
			input := "";
			state := st
	done

let parse_responses inp =
	let input = ref inp in
	let state = ref (Response_header.init_state ()) in
	while String.length !input > 0 do
		match Response_header.parse !state !input with
		| Response_header.Result (r, consumed) ->
			print_response r;
			input := String.sub !input consumed ((String.length !input) - consumed);
			(* Check for payload *)
			(match Payload.init_from_response r with
			| Payload.No_payload ->
				Printf.printf " No payload.\n"
			| Payload.Error s ->
				Printf.printf " Error: %s\n" s; exit 1
			| Payload.Payload state ->
				(match Payload.parse state !input with
				 | Payload.Result (p, consumed) ->
					print_payload (Payload.serialize_of_response r) p;
				 	input := String.sub !input consumed ((String.length !input) - consumed);
				 | Payload.Parse_incomplete s ->
					(* We might have to simulate a Connection close. *)
					Payload.connection_closed s;
					(match Payload.get_parse_result s with
					 | None ->
						Printf.printf " Payload incomplete!\n"
					 | Some p ->
						print_payload (Payload.serialize_of_response r) p
					);
					input := ""
				)
			);
			state := Response_header.init_state ()
		| Response_header.Parse_incomplete st ->
			if Response_header.num_bytes_parsed st > 0 then
				Printf.printf "Response incomplete!\n";
			input := "";
			state := st
	done

let compact_parse_requests inp =
	let input = ref inp in
	let state = ref (Request.init_state ()) in
	while String.length !input > 0 do
		match Request.parse !state !input with
		| Request.Result (r, consumed) ->
			let buf = Buffer.create 512 in
			Request.serialize buf r;
			Printf.printf "%s" (Buffer.contents buf);
			input := String.sub !input consumed ((String.length !input) - consumed);
			state := Request.init_state ()
		| Request.Parse_incomplete st ->
			if Request.num_bytes_parsed st > 0L then
				Printf.printf "Request incomplete!\n";
			input := "";
			state := st
		| Request.Error s ->
			Printf.printf "Error: %s!\n" s;
			input := ""
	done

let compact_parse_responses inp =
	let input = ref inp in
	let state = ref (Response.init_state ()) in
	while String.length !input > 0 do
		match Response.parse !state !input with
		| Response.Result (r, consumed) ->
			let buf = Buffer.create 512 in
			Response.serialize buf r;
			Printf.printf "%s" (Buffer.contents buf);
			input := String.sub !input consumed ((String.length !input) - consumed);
			state := Response.init_state ()
		| Response.Parse_incomplete st ->
			if Response.num_bytes_parsed st > 0L then
				Printf.printf "Response incomplete!\n";
			input := "";
			state := st
		| Response.Error s ->
			Printf.printf "Error: %s!\n" s;
			input := ""
	done



let print_exception e =
	let msg =
		match e with
		| Headers.Http_error e ->
			Printf.sprintf "Header parsing error: %s\n"
				(Headers.string_of_error e)
		| Request_header.Http_error e ->
			Printf.sprintf "Request parsing error: %s\n"
				(Request_header.string_of_error e)
		| Response_header.Http_error e ->
			Printf.sprintf "Response parsing error: %s\n"
				(Response_header.string_of_error e)
		| Payload.Http_error e ->
			Printf.sprintf "Payload parsing error: %s\n"
				(Payload.string_of_error e)
		| Sys_error s ->
			Printf.sprintf "%s" s
		| e ->
			Printf.sprintf "%s" (Printexc.to_string e)
	in
	Printf.eprintf "%s\n" msg

let _ =
	let input_file = parse_args () in
	try
		let ic = open_in input_file in
		let input = read_whole_file ic in
		if !do_requests
		then (if !test_compact_api
		      then compact_parse_requests
		      else parse_requests) input
		else (if !test_compact_api
		      then compact_parse_responses
		      else parse_responses) input;
		close_in ic;
		exit 0
	with e ->
		print_exception e;
		exit 1
