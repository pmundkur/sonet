(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-2012  Prashanth Mundkur.                           *)
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
module U = Unix
module C = Http_client
module HC = Http_client.Make(struct type t = unit end)

let string_of_resp resp =
  let buf = Buffer.create 1024 in
  H.Response.serialize buf resp;
  Buffer.contents buf

let string_of_req = function
  | C.Payload (_, _)  -> "payload"
  | C.FileRecv (_, _) -> "download"
  | C.FileSend (_, _) -> "upload"

let funopt f = function
  | None   -> ()
  | Some e -> f e

let errmsg (u, e) =
  Printf.printf "\t%s: %s\n" u (C.string_of_error e)

let show_result verbose r =
  Printf.printf "\n%s %s: " (H.string_of_meth r.HC.meth) r.HC.url;
  (match r.HC.response with
    | C.Failure (one, rest) ->
      Printf.printf "No response\n";
      errmsg one; List.iter errmsg rest
    | C.Success (resp, errs) ->
      Printf.printf "Response received\n";
      if verbose || r.HC.meth = H.Head then Printf.printf "%s\n" (string_of_resp resp);
      List.iter errmsg errs
  )

let rec show_results verbose = function
  | [] -> ()
  | r :: rest ->
    show_result verbose r;
    show_results verbose rest

let print_usage () =
  Printf.printf "%s: [options]\n" Sys.argv.(0);
  Printf.printf "    -head url+ : print the headers of the url\n";
  Printf.printf "    -get url+ : print the contents of the url\n";
  Printf.printf "    -save file url+ : save the contents of the url to a file\n";
  Printf.printf "    (-put|-post) file url+ : put the contents of the file at the url\n";
  Printf.printf "    -p : show payload\n";
  exit 1

let check_supported_url url =
  if not (C.is_supported_url url)
  then (Printf.eprintf "Unsupported url: %s\n" url; exit 1)

let run () =
  let reqs = ref [] in
  let verbose = ref false in
  let get_url urls =
    List.iter check_supported_url urls;
    reqs := (H.Get, C.Payload (urls, None), ()) :: !reqs in
  let head_url urls =
    List.iter check_supported_url urls;
    reqs := (H.Head, C.Payload (urls, None), ()) :: !reqs in
  let send_url meth urls filename =
    List.iter check_supported_url urls;
    let fd = U.openfile filename [U.O_RDONLY] 0 in
      reqs := (meth, C.FileSend (urls, fd), ()) :: !reqs in
  let save_url urls filename =
    List.iter check_supported_url urls;
    let fd = U.openfile filename [U.O_WRONLY; U.O_CREAT; U.O_TRUNC] 0o640 in
      reqs := (H.Get, C.FileRecv (urls, fd), ()) :: !reqs in
  let num_args = Array.length Sys.argv in
  let get_arg opt indx =
    if indx >= num_args
    then (Printf.printf "Insufficient args for %s\n" opt; exit 1)
    else Sys.argv.(indx) in
  let meth_of_opt = function
    | "-post" -> H.Post | _ -> H.Put in
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
              | "-head" ->
                let urls, next = get_url_args (indx + 1) in
                head_url urls;
                process_args next
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
                verbose := true;
                process_args (indx + 1)
              | _ ->
                Printf.printf "Unrecognized option: %s\n" opt;
                print_usage ()
         )
  in
  process_args 1;
  let results = HC.request !reqs
  in show_results !verbose results

let _ =
  if Array.length Sys.argv == 1 then
    print_usage ();
  Printexc.record_backtrace true;
  try run ()
  with
    | C.Invalid_url (url, e) ->
      Printf.eprintf "Invalid url %s: %s\n%!" url e
    | C.Invalid_request e ->
      Printf.eprintf "Invalid request: %s!\n%!" e
    | e ->
      Printf.eprintf "%s\n%s\n%!"
        (Printexc.to_string e)
        (Printexc.get_backtrace ())
