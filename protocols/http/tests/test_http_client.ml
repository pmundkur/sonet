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

let string_of_req = function
  | Client.Payload (_, _) -> "payload"
  | Client.FileRecv (_, _) -> "download"
  | Client.FileSend (_, _) -> "upload"

let errmsg (u, e) =
  let em = match e with
    | Client.Unix e -> Unix.error_message e
    | Client.Http s -> Printf.sprintf "HTTP error response %d" s
    | Client.Other m -> m
  in Printf.printf "\t%s: %s\n" u em

let funopt f = function
  | None -> ()
  | Some e -> f e

let show_result verbose r =
  Printf.printf "\n%s %s: " (R.string_of_meth r.Client.meth) r.Client.url;
  (match r.Client.response with
     | None ->
         Printf.printf "No response\n"
     | Some resp ->
         Printf.printf "Response received\n";
         if verbose then Printf.printf "%s\n" (string_of_resp resp);
  );
  funopt (fun el -> List.iter errmsg el) r.Client.error

let rec show_results verbose = function
  | [] -> ()
  | r :: rest ->
      show_result verbose r;
      show_results verbose rest

let print_usage () =
  Printf.printf "%s: [options]\n" Sys.argv.(0);
  Printf.printf "    -get url+ : print the contents of the url\n";
  Printf.printf "    -save file url+ : save the contents of the url to a file\n";
  Printf.printf "    (-put|-post) file url+ : put the contents of the file at the url\n";
  Printf.printf "    -p : show payload\n";
  exit 1

let check_supported_url url =
  if not (Client.is_supported_url url)
  then (Printf.eprintf "Unsupported url: %s\n" url; exit 1)

let run () =
  let reqs = ref [] in
  let verbose = ref false in
  let get_url urls =
    List.iter check_supported_url urls;
    reqs := (R.Get, Client.Payload (urls, None)) :: !reqs in
  let send_url meth urls filename =
    List.iter check_supported_url urls;
    let fd = U.openfile filename [U.O_RDONLY] 0 in
      reqs := (meth, Client.FileSend (urls, fd)) :: !reqs in
  let save_url urls filename =
    List.iter check_supported_url urls;
    let fd = U.openfile filename [U.O_WRONLY; U.O_CREAT; U.O_TRUNC] 0o640 in
      reqs := (R.Get, Client.FileRecv (urls, fd)) :: !reqs in
  let num_args = Array.length Sys.argv in
  let get_arg opt indx =
    if indx >= num_args
    then (Printf.printf "Insufficient args for %s\n" opt; exit 1)
    else Sys.argv.(indx) in
  let meth_of_opt = function
    | "-post" -> R.Post | _ -> R.Put in
  let is_opt a = a.[0] = '-' in
  let rec get_url_args indx =
    let rec helper i acc =
      if i >= num_args || is_opt Sys.argv.(i)
      then List.rev acc, i
      else helper (i+1) (Sys.argv.(i) :: acc)
    in helper indx [] in
  let rec process_args indx =
    if indx >= num_args then ()
    else (let opt = Sys.argv.(indx) in
            match opt with
              | "-get" ->
                  let urls, next = get_url_args (indx + 1) in
                    get_url urls;
                    process_args next
              | "-put" | "-post" ->
                  let meth = meth_of_opt opt in
                  let file = get_arg opt (indx + 1) in
                  let urls, next = get_url_args (indx + 2) in
                    send_url meth urls file;
                    process_args next
              | "-save" ->
                  let file = get_arg opt (indx + 1) in
                  let urls, next = get_url_args (indx + 2) in
                    save_url urls file;
                    process_args next
              | "-h" | "--help" ->
                  print_usage ()
              | "-p" | "--payload" ->
                  verbose := true
              | _ ->
                  Printf.printf "Unrecognized option: %s\n" opt;
                  print_usage ()
         )
  in
    process_args 1;
    let results = Client.request !reqs
    in show_results !verbose results

let _ =
  if Array.length Sys.argv == 1 then
    print_usage ();
  Printexc.record_backtrace true;
  try run ()
  with
    | Client.Invalid_url (url, e) ->
        Printf.eprintf "Invalid url %s: %s\n%!" url e
    | Client.Invalid_request r ->
        Printf.eprintf "Invalid %s request: no urls!\n%!" (string_of_req r)
    | e ->
        Printf.eprintf "%s\n%s\n%!"
          (Printexc.to_string e)
          (Printexc.get_backtrace ())
