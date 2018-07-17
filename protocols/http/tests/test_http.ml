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

type stream =
  | Requests
  | Responses

let stream = ref None
let set_stream typ () = stream := Some typ

let test_compact_api = ref false
let show_summary = ref false
let show_headers = ref false
let show_payload = ref false

let parse_args () =
  let options = [("-rq", Arg.Unit (set_stream Requests), " parse request stream");
                 ("-rp", Arg.Unit (set_stream Responses), " parse response stream");
                 ("-c", Arg.Set test_compact_api, " test compact api");
                 ("-s", Arg.Set show_summary, " show summary");
                 ("-ph", Arg.Set show_headers, " show headers");
                 ("-pp", Arg.Set show_payload, " show payload (only for non-compact!)")
                ] in
  let file = ref None in
  let usage = Printf.sprintf "Usage: %s [options] file" Sys.argv.(0) in
    Arg.parse (Arg.align options) (fun f -> file := Some f) usage;
    match !file, !stream with
      | None, _ ->
          Arg.usage (Arg.align options) usage; exit 1
      | _, None ->
          Printf.eprintf "Please specify whether the input contains HTTP requests or responses.\n";
          Arg.usage (Arg.align options) usage; exit 1
      | Some f, Some s -> f, s

let read_whole_file input_file =
  let st = Unix.stat input_file in
  let buf = Bytes.create st.Unix.st_size in
  let fd = Unix.openfile input_file [ Unix.O_RDONLY ] 0 in
  let rec do_read start len =
    if len = 0 then ()
    else
      match Unix.read fd buf start len with
        | 0 -> raise End_of_file
        | read -> do_read (start + read) (len - read)
  in
    do_read 0 st.Unix.st_size;
    Unix.close fd;
    buf

let messages = ref 0

let process_request req =
  incr messages;
  if !show_headers then begin
    let buf = Buffer.create 512 in
      Request_header.serialize buf req;
      Printf.printf "%s" (Buffer.contents buf)
  end else if !show_summary then
    Printf.printf "%s %s\n"
      (string_of_meth req.Request_header.meth)
      (Request_header.string_of_url req.Request_header.url)

let process_response resp =
  incr messages;
  if !show_headers then begin
    let buf = Buffer.create 512 in
      Response_header.serialize buf resp;
      Printf.printf "%s" (Buffer.contents buf)
  end else if !show_summary then
    Printf.printf "%d %s\n"
      resp.Response_header.status_code
      resp.Response_header.reason_phrase

let process_payload serializer p =
  if !show_payload then
    let buf = Buffer.create 512 in
      serializer buf p;
      Printf.printf "%s" (Buffer.contents buf)
  else if !show_summary then
    Printf.printf " Payload: %d bytes.\n"
      (Buffer.length p.Payload.content)

let parse_requests input =
  let ofs, rem = ref 0, ref (String.length input) in
  let state = ref (Request_header.init_state ()) in
    while !rem > 0 do
      match Request_header.parse_substring !state input !ofs !rem with
        | Request_header.Result (r, consumed) ->
            process_request r;
            ofs := !ofs + consumed;
            rem := !rem - consumed;
            (* Check for payload *)
            (match Payload.init_from_request r with
               | Payload.No_payload ->
                   if !show_summary then
                     Printf.printf " No payload.\n"
               | Payload.Error s ->
                   Printf.printf " Error: %s\n" s; exit 1
               | Payload.Payload state ->
                   (match Payload.parse_substring state input !ofs !rem with
                      | Payload.Result (p, consumed) ->
                          process_payload (Payload.serialize_of_request r) p;
                          ofs := !ofs + consumed;
                          rem := !rem - consumed;
                      | Payload.Parse_incomplete s ->
                          (* We might have to simulate a Connection close. *)
                          Payload.connection_closed s;
                          (match Payload.get_parse_result s with
                             | None ->
                                 Printf.printf " Payload incomplete!\n"
                             | Some p ->
                                 process_payload (Payload.serialize_of_request r) p
                          );
                          rem := 0
                   )
            );
            state := Request_header.init_state ()
        | Request_header.Parse_incomplete st ->
            if Request_header.num_bytes_parsed st > 0 then
              Printf.printf "Request incomplete!\n";
            rem := 0;
            state := st
    done

let parse_responses input =
  let ofs, rem = ref 0, ref (String.length input) in
  let state = ref (Response_header.init_state ()) in
    while !rem > 0 do
      match Response_header.parse_substring !state input !ofs !rem with
        | Response_header.Result (r, consumed) ->
            process_response r;
            ofs := !ofs + consumed;
            rem := !rem - consumed;
            (* Check for payload *)
            (match Payload.init_from_response r with
               | Payload.No_payload ->
                   if !show_summary then
                     Printf.printf " No payload.\n"
               | Payload.Error s ->
                   Printf.printf " Error: %s\n" s; exit 1
               | Payload.Payload state ->
                   (match Payload.parse_substring state input !ofs !rem with
                      | Payload.Result (p, consumed) ->
                          process_payload (Payload.serialize_of_response r) p;
                          ofs := !ofs + consumed;
                          rem := !rem - consumed;
                      | Payload.Parse_incomplete s ->
                          (* We might have to simulate a Connection close. *)
                          Payload.connection_closed s;
                          (match Payload.get_parse_result s with
                             | None ->
                                 Printf.printf " Payload incomplete!\n"
                             | Some p ->
                                 process_payload (Payload.serialize_of_response r) p
                          );
                          rem := 0
                   )
            );
            state := Response_header.init_state ()
        | Response_header.Parse_incomplete st ->
            if Response_header.num_bytes_parsed st > 0 then
              Printf.printf "Response incomplete!\n";
            rem := 0;
            state := st
    done

let compact_parse_requests input =
  let ofs, rem = ref 0, ref (String.length input) in
  let state = ref (Request.init_state ()) in
    while !rem > 0 do
      match Request.parse_substring !state input !ofs !rem with
        | Request.Result (r, consumed) ->
            if !show_payload || !show_headers || !show_payload then begin
              let buf = Buffer.create 512 in
                Request.serialize buf r;
                Printf.printf "%s" (Buffer.contents buf)
            end;
            incr messages;
            ofs := !ofs + consumed;
            rem := !rem - consumed;
            state := Request.init_state ()
        | Request.Parse_incomplete st ->
            if Request.num_bytes_parsed st > 0L then
              Printf.printf "Request incomplete!\n";
            rem := 0;
            state := st
        | Request.Error s ->
            Printf.printf "Error: %s!\n" s;
            rem := 0
    done

let compact_parse_responses input =
  let ofs, rem = ref 0, ref (String.length input) in

  (* We can't really parse responses without knowing the corresponding
     request, due to HEAD. We just fake a non-HEAD request for this test. *)
  let fake_req = {Request_header.version = HTTP11; meth = Get; url = Request_header.Star; headers = []} in
  let state = ref (Response.init_state fake_req) in
    while !rem > 0 do
      match Response.parse_substring !state input !ofs !rem with
        | Response.Result (r, consumed) ->
            if !show_payload || !show_headers || !show_payload then begin
              let buf = Buffer.create 512 in
                Response.serialize buf r;
                Printf.printf "%s" (Buffer.contents buf)
            end;
            incr messages;
            ofs := !ofs + consumed;
            rem := !rem - consumed;
            state := Response.init_state fake_req
        | Response.Parse_incomplete st ->
            if Response.num_bytes_parsed st > 0L then
              Printf.printf "Response incomplete!\n";
            rem := 0;
            state := st
        | Response.Error s ->
            Printf.printf "Error: %s!\n" s;
            rem := 0
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
  let input_file, stream = parse_args () in
    try
      let input = read_whole_file input_file in
      let start = Unix.times () in
      let _ =
        (match stream with
           | Requests ->
               (if !test_compact_api
                then compact_parse_requests
                else parse_requests) input
           | Responses ->
               (if !test_compact_api
                then compact_parse_responses
                else parse_responses) input) in
      let finish = Unix.times () in
        Printf.printf "%d messages processed in %f user %f system times.\n"
          !messages (finish.Unix.tms_utime -. start.Unix.tms_utime)
          (finish.Unix.tms_stime -. start.Unix.tms_utime);
        exit 0
    with e ->
      print_exception e;
      exit 1
