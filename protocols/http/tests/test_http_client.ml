(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010      Prashanth Mundkur.                            *)
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

module H = Http
module R = Http.Request_header
module Client = Http_client
module U = Unix

let string_of_resp resp =
  let buf = Buffer.create 1024 in
    H.Response.serialize buf resp;
    Buffer.contents buf

let rec show_results = function
  | [] -> ()
  | r :: rest ->
      let resmsg =
        (match r.Client.error with
           | Some (Client.Unix e) -> Unix.error_message e
           | Some (Client.Other m) -> m
           | None ->
               (match r.Client.response with
                  | None ->
                      "No response"
                  | Some resp ->
                      Printf.sprintf "\n%s\n" (string_of_resp resp)
               )
        ) in
        Printf.printf "%s %s: %s\n%!"
          (R.string_of_meth r.Client.meth) r.Client.url resmsg;
        show_results rest

let print_usage () =
  Printf.printf "%s: [options]\n" Sys.argv.(0);
  Printf.printf "    -get url : print the contents of the url\n";
  Printf.printf "    -save url file : save the contents of the url to a file\n";
  Printf.printf "    (-put|-post) url file : put the contents of the file at the url\n";
  exit 1

let check_supported_url url =
  if not (Client.is_supported_url url)
  then (Printf.eprintf "Unsupported url: %s\n" url; exit 1)

let run () =
  let reqs = ref [] in
  let get_url url =
    check_supported_url url;
    reqs := (R.Get, Client.Payload (url, None)) :: !reqs in
  let send_url meth url filename =
    check_supported_url url;
    let fd = U.openfile filename [U.O_RDONLY] 0 in
      reqs := (meth, Client.FileSend (url, fd)) :: !reqs in
  let save_url url filename =
    check_supported_url url;
    let fd = U.openfile filename [U.O_WRONLY; U.O_CREAT; U.O_TRUNC] 0o640 in
      reqs := (R.Get, Client.FileRecv (url, fd)) :: !reqs in
  let num_args = Array.length Sys.argv in
  let get_arg opt indx =
    if indx >= num_args
    then (Printf.printf "Insufficient args for %s\n" opt; exit 1)
    else Sys.argv.(indx) in
  let meth_of_opt = function
    | "-post" -> R.Post | _ -> R.Put
  in
  let rec process_args indx =
    if indx >= num_args then ()
    else (let opt = Sys.argv.(indx) in
            match opt with
              | "-get" ->
                  let url = get_arg opt (indx + 1) in
                    get_url url;
                    process_args (indx + 2)
              | "-put" | "-post" ->
                  let meth = meth_of_opt opt in
                  let url = get_arg opt (indx + 1) in
                  let file = get_arg opt (indx + 2) in
                    send_url meth url file;
                    process_args (indx + 3)
              | "-save" ->
                  let url = get_arg opt (indx + 1) in
                  let file = get_arg opt (indx + 2) in
                    save_url url file;
                    process_args (indx + 3)
              | "-h" | "--help" ->
                  print_usage ()
              | _ ->
                  Printf.printf "Unrecognized option: %s\n" opt;
                  print_usage ()
         )
  in
    process_args 1;
    let results = Client.request !reqs
    in show_results results

let _ =
  Printexc.record_backtrace true;
  try run ()
  with
    | Client.Invalid_url (url, e) ->
        Printf.eprintf "Invalid url %s: %s\n%!" url e
    | e ->
        Printf.eprintf "%s\n%s\n%!"
          (Printexc.to_string e)
          (Printexc.get_backtrace ())
