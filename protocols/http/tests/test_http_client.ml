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

let rec show_results = function
  | [] -> ()
  | r :: rest ->
      let resmsg =
        (match r.Client.error with
           | Some (Client.Unix e) -> Unix.error_message e
           | Some (Client.Other m) -> m
           | None -> "Success"
        ) in
        Printf.printf "%s %s: %s\n%!" (R.string_of_meth r.Client.meth) r.Client.url resmsg;
        show_results rest

let run () =
  let reqs = (List.fold_left
                (fun acc url -> (R.Get, Client.Payload (url, None)) :: acc)
                [] (List.tl (Array.to_list Sys.argv))) in
  let results = Client.make_requests reqs
  in show_results results

let _ =
  Printexc.record_backtrace true;
  try run ()
  with e ->
    Printf.eprintf "%s\n%s\n%!"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
