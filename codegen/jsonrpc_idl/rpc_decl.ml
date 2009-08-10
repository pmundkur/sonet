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

open Syntax

type decl =
  | Rpc_use of use
  | Rpc_server of server
  | Rpc_rpc of rpc
  | Rpc_endpoint of endpoint

type elem = Server | RPC | Endpoint

let elem_name = function Server -> "server" | RPC -> "rpc" | Endpoint -> "endpoint"

type spec = {
  uses: string list;
  servers: server list;
  rpcs: rpc list;
  endpoints: endpoint list;
}

let init_spec = {
  uses = [];
  servers = [];
  rpcs = [];
  endpoints = [];
}

let get_rpcs_by_server spec server =
  List.rev (List.filter (fun r -> r.rpc_server = server.server_name) spec.rpcs)

let get_sorted_rpcs_by_server spec server =
  let rpcs = List.filter (fun r -> r.rpc_server = server.server_name) spec.rpcs in
  let rlist, nlist =
    List.fold_left (fun (rlist, nlist) rpc ->
                      match rpc.rpc_response with
                        | None -> rlist, rpc :: nlist
                        | Some r -> (rpc, r) :: rlist, nlist
                   ) ([], []) rpcs
  in rlist, nlist

let get_matching_servers spec names =
  List.filter (fun s -> List.mem s.server_name names) spec.servers

let get_endpoint_by_name spec name =
  List.find (fun e -> e.endpoint_name = name) spec.endpoints

exception Multiple_decl of elem * string
exception Unknown_ref of elem * string
exception Unknown_RPC_type of string
exception Notification_has_response of string
exception RPC_needs_response of string

let present spec elem name =
  try
    match elem with
      | Server -> ignore (List.find (fun s -> s.server_name = name) spec.servers); true
      | RPC    -> ignore (List.find (fun r -> (r.rpc_server ^ "." ^ r.rpc_request.request_name) = name) spec.rpcs); true
      | Endpoint -> ignore (List.find (fun e -> e.endpoint_name = name) spec.endpoints); true
  with Not_found -> false

let check_new spec elem name =
  if present spec elem name then raise (Multiple_decl (elem, name))

let check_existing spec elem name =
  if not (present spec elem name) then raise (Unknown_ref (elem, name))

let add_use spec u =
  List.fold_left (fun spec m ->
                    if not (List.mem m spec.uses)
                    then { spec with uses = m :: spec.uses }
                    else spec
                 ) spec u.use_modules

let get_uses spec = List.rev spec.uses

let add_server spec s =
  check_new spec Server s.server_name;
  { spec with servers = s :: spec.servers }

let get_servers spec = List.rev spec.servers

let add_rpc spec r =
  let name = r.rpc_request.request_name in
    check_new spec RPC (r.rpc_server ^ "." ^ name);
    check_existing spec Server r.rpc_server;
    (match r.rpc_type with
       | "notification" | "Notification" ->
           if r.rpc_response <> None then
             raise (Notification_has_response name)
       | "rpc" | "RPC" ->
           if r.rpc_response = None then
             raise (RPC_needs_response name)
       | s -> raise (Unknown_RPC_type s));
    { spec with rpcs = r :: spec.rpcs }

let add_endpoint spec e =
  check_new spec Endpoint e.endpoint_name;
  List.iter (fun s -> check_existing spec Server s) e.endpoint_servers;
  List.iter (fun c -> check_existing spec Server c) e.endpoint_clients;
  { spec with endpoints = e :: spec.endpoints }

let get_endpoints spec = List.rev spec.endpoints

let add_decl spec = function 
  | Rpc_use u -> add_use spec u
  | Rpc_server s -> add_server spec s
  | Rpc_rpc r -> add_rpc spec r
  | Rpc_endpoint e -> add_endpoint spec e

let error_message e =
  match e with
    | Multiple_decl (e, n) ->
        Printf.sprintf  "Repeated declaration of %s \"%s\"" (elem_name e) n
    | Unknown_ref (e, n) ->
        Printf.sprintf  "Reference to unknown %s \"%s\"" (elem_name e) n
    | Notification_has_response n ->
        Printf.sprintf  "Notification \"%s\" cannot specify a response" n
    | RPC_needs_response n ->
        Printf.sprintf  "RPC \"%s\" needs a response specification" n
    | e -> raise e

let spec_with_decls decls =
  try
    List.fold_left (fun spec d -> add_decl spec d) init_spec decls
  with e ->
    Printf.eprintf "%s\n" (error_message e);
    exit 1
