(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License (version
 * 2.1 only) as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let do_print = ref true

let parse_args () =
  let options = [("-no-print-value", Arg.Clear do_print, " no output")] in
  let file = ref None in
  let usage = Printf.sprintf "Usage: %s [options] file" Sys.argv.(0) in
    Arg.parse (Arg.align options) (fun f -> file := Some f) usage;
    match !file with
      | Some f -> f
      | None -> Arg.usage (Arg.align options) usage; exit 1

let read_whole_file ic =
  let buf = Buffer.create 512 in
  let rec do_read () =
    try
      let line = input_line ic in
        Buffer.add_string buf line;
        do_read ()
    with End_of_file ->
      Buffer.contents buf
  in do_read ()

let parse_file f =
  let ic = open_in f in
  let input = ref (read_whole_file ic) in
  let state = ref (Json_parse.init_parse_state ()) in
    while String.length !input > 0 do
      match Json_parse.parse !state !input with
        | Json_parse.Json_value (v, consumed) ->
            if !do_print then
              Printf.printf "%s\n" (Json.to_string v);
            input := String.sub !input consumed ((String.length !input) - consumed);
            state := Json_parse.init_parse_state ()
        | Json_parse.Json_parse_incomplete st ->
            input := "";
            state := st
    done;
    match Json_parse.finish_parse !state with
      | Some v -> Printf.printf "%s\n" (Json.to_string v)
      | None -> ()

let print_exception e =
  let msg =
    match e with
      | Json_parse.Parse_error e ->
          Json_parse.string_of_error e
      | Sys_error s ->
          Printf.sprintf "%s" s
      | e ->
          Printf.sprintf "%s" (Printexc.to_string e)
  in
    if !do_print then
      Printf.eprintf "%s\n" msg

let is_internal_error = function
  | Json_parse.Parse_error (Json_parse.Internal_error _) -> true
  | _ -> false

let _ =
  let input_file = parse_args () in
    try
      parse_file input_file;
      exit 0
    with e ->
      print_exception e;
      if is_internal_error e then exit 255 else exit 1
