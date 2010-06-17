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
open Rpc_decl
open Format

module Type_conv = struct
  let is_base_type = function
    | "string" | "int" | "int64" | "float" | "bool" -> true
    | _ -> false

  let to_json t =
    if is_base_type t then Printf.sprintf "Json_conv.%s_to_json" t
    else t ^ "_to_json"

  let of_json t =
    if is_base_type t then Printf.sprintf "Json_conv.%s_of_json" t
    else t ^ "_of_json"
end


type var = { stem: string; mark: int }

let name_of_var v =
  match v.mark with
    | 0 -> Printf.sprintf "%s" v.stem
    | d -> Printf.sprintf "%s_%d" v.stem d

module Var_env = struct
  module StringMap = Map.Make (struct type t = string let compare = compare end)

  type name_entry = { cur_mark: int; entries: var list; }

  let new_name_entry = { cur_mark = 0; entries = [] }

  let make_new_var name_entry name =
    let var = { stem = name; mark = name_entry.cur_mark} in
      var, { cur_mark = var.mark + 1; entries = var :: name_entry.entries }

  type t = name_entry StringMap.t
  let new_env = StringMap.empty

  let new_var env full_name =
    let var, new_entry = make_new_var (try StringMap.find full_name env
                                       with Not_found -> new_name_entry) full_name in
      var, (StringMap.add full_name new_entry env)

  let new_ident_from_name env ?(api = false) ?(prefix="") ?(suffix="") stem =
    new_var env ((if api then "" else "_") ^ prefix ^ stem ^ suffix)

  let new_idents_from_names env ?(prefix="") ?(suffix="") names =
    let vlist, env =
      List.fold_left (fun (vlist, env) n ->
                        let v, env' = new_ident_from_name env ~prefix ~suffix n in
                          (v :: vlist), env'
                     ) ([], env) names in
      (List.rev vlist), env
end


module Server = struct
  let start_server ff s =
    fprintf ff "module %s =@\n" (String.capitalize s.server_name);
    fprintf ff "@[<v 8>struct@,"

  let end_server ff =
    fprintf ff "@]@\nend@\n@\n@?"

  let gen_dispatch_struct ff server rpc_list notif_list =
    let get_arg_types params =
      match params with
        | [] -> [ "unit" ]
        |  _ -> List.map (fun p -> p.param_type) params
    in
    let sig_name = (String.lowercase server.server_name) ^ "_impl" in
      fprintf ff "type 'a %s =@\n" sig_name;
      fprintf ff "@[<v 8>{@,";
      if List.length rpc_list > 0 then
        fprintf ff "(* RPCs *)";
      List.iter (fun (rpc, resp) ->
                   let sg = get_arg_types rpc.rpc_request.request_params in
                   let sg = sg @ [ "'a"; resp.response_value.param_type ] in
                     fprintf ff "@,%s: %s;" rpc.rpc_request.request_handler (String.concat " -> " sg)
                ) rpc_list;
      if List.length notif_list > 0 then
        (if List.length rpc_list > 0
         then fprintf ff "@,@,(* Notifications *)"
         else fprintf ff "(* Notifications *)");
      List.iter (fun n ->
                   let sg = get_arg_types n.rpc_request.request_params in
                   let sg = sg @ [ "'a"; "unit" ] in
                     fprintf ff "@,%s: %s;" n.rpc_request.request_handler (String.concat " -> " sg)
                ) notif_list;
      (match server.server_message_filter with
         | Some f ->
             fprintf ff "@,@,(* Message filter *)";
             fprintf ff "@,%s: string -> 'a -> unit;" f
         | None -> ()
      );
      fprintf ff "@,@,(* Exception error handler *)";
      fprintf ff "@,%s: exn -> 'a -> Jsonrpc.rpc_error; " server.server_error_handler;
      fprintf ff "@]@\n}@\n@\n";
      sig_name

  let gen_messages_handled_list ff rpc_list notif_list =
    if List.length rpc_list = 0 then
      fprintf ff "let rpcs_handled = []@,@,"
    else begin
      fprintf ff "@[<v 8>let rpcs_handled = [";
      List.iter (fun r ->
                   fprintf ff "@,\"%s\";" (fst r).rpc_request.request_name
                ) rpc_list;
      fprintf ff "@]@,]@,@,"
    end;
    if List.length notif_list = 0 then
      fprintf ff "let notifications_handled = []@,@,"
    else begin
      fprintf ff "@[<v 8>let notifications_handled = [";
      List.iter (fun n ->
                   fprintf ff "@,\"%s\";" n.rpc_request.request_name
                ) notif_list;
      fprintf ff "@]@,]@,@,"
    end

  let gen_param ff venv otvn _i p =
    let arg, venv = Var_env.new_ident_from_name venv p.param_name in
      fprintf ff "let %s = %s (Json_conv.get_object_field %s \"%s\") in@," (name_of_var arg) (Type_conv.of_json p.param_type) otvn p.param_name;
      arg, venv

  let gen_request ff venv reqv impl_module cbvn rpc resp =
    let otv, venv = Var_env.new_ident_from_name venv "params" in
    let otvn, reqvn = name_of_var otv, name_of_var reqv in
    let methname = rpc.rpc_request.request_handler in
    let params = rpc.rpc_request.request_params in
      fprintf ff "@[<v 8>| \"%s\" ->@," rpc.rpc_request.request_name;
      let args_str, venv =
        (match params with
           | [] ->
               "()", venv
           | _  ->
               fprintf ff "let %s = Json_conv.get_object_table %s.Jsonrpc.params in@," otvn reqvn;
               let paramsv, venv, _ =
                 List.fold_left (fun (alist, venv, i) p ->
                                   let a, venv = gen_param ff venv otvn i p in
                                     (a :: alist), venv, (i + 1)
                                ) ([], venv, 0) params in
                 (String.concat " " (List.map (fun v -> name_of_var v) (List.rev paramsv))), venv
        )
      in
      let respv, venv = Var_env.new_ident_from_name venv "resp" in
      let respjv, _venv = Var_env.new_ident_from_name venv "resp_j" in
      let respvn, respjvn = name_of_var respv, name_of_var respjv in
        fprintf ff "let %s = %s.%s %s %s in@," respvn impl_module methname args_str cbvn;
        fprintf ff "let %s = %s %s in@," respjvn (Type_conv.to_json resp.response_value.param_type) respvn;
        fprintf ff "Jsonrpc.Result %s@]@," respjvn

  let gen_notification ff venv reqv impl_module cbvn rpc =
    let otv, venv = Var_env.new_ident_from_name venv "params" in
    let otvn, reqvn = name_of_var otv, name_of_var reqv in
    let methname = rpc.rpc_request.request_handler in
    let params = rpc.rpc_request.request_params in
      fprintf ff "@[<v 8>| \"%s\" ->@," rpc.rpc_request.request_name;
      let args_str, _venv =
        (match params with
           | [] ->
               "()", venv
           | _  ->
               fprintf ff "let %s = Json_conv.get_object_table %s.Jsonrpc.params in@," otvn reqvn;
               let paramsv, venv, _ =
                 List.fold_left (fun (alist, venv, i) p ->
                                   let a, venv = gen_param ff venv otvn i p in
                                     (a :: alist), venv, (i + 1)
                                ) ([], venv, 0) params in
                 (String.concat " " (List.map (fun v -> name_of_var v) (List.rev paramsv))), venv
        )
      in
        fprintf ff "%s.%s %s %s@]@," impl_module methname args_str cbvn

  let gen_notification_dispatch ff venv _server impl_module nlist =
    let dispv, venv = Var_env.new_ident_from_name ~api:true venv "dispatch_notification" in
    let reqv, venv = Var_env.new_ident_from_name venv "req" in
    let cbv, venv = Var_env.new_ident_from_name venv "callback_arg" in
    let implv, venv = Var_env.new_ident_from_name venv impl_module in
    let reqvn, cbvn, implvn = name_of_var reqv, name_of_var cbv, name_of_var implv in
      fprintf ff "@[<v 8>let %s %s %s %s =@," (name_of_var dispv) implvn reqvn cbvn;
      fprintf ff "match %s.Jsonrpc.method_name with@," reqvn;
      List.iter (fun n -> gen_notification ff venv reqv implvn cbvn n) nlist;
      fprintf ff "| _ -> raise (Jsonrpc.Unknown_request %s.Jsonrpc.method_name)@]@,@\n" reqvn

  let gen_rpc_dispatch ff venv server impl_module rpcs =
    let dispv, venv = Var_env.new_ident_from_name ~api:true venv "dispatch_rpc" in
    let reqidjv, venv = Var_env.new_ident_from_name venv "req_id_j" in
    let reqv, venv = Var_env.new_ident_from_name venv "req" in
    let implv, venv = Var_env.new_ident_from_name venv impl_module in
    let pv, venv = Var_env.new_ident_from_name venv "payload" in
    let cbv, venv = Var_env.new_ident_from_name venv "callback_arg" in
    let reqidjvn, reqvn, implvn = name_of_var reqidjv, name_of_var reqv, name_of_var implv in
    let pvn, cbvn = name_of_var pv, name_of_var cbv in
      fprintf ff "@[<v 8>let %s %s %s %s %s =@," (name_of_var dispv) implvn reqidjvn reqvn cbvn;
      (match server.server_message_filter with
         | Some f -> fprintf ff "%s.%s %s.Jsonrpc.method_name %s;@," implvn f reqvn cbvn
         | None -> ()
      );
      fprintf ff "@[<v 8>let %s =@," pvn;
      fprintf ff "@[<v 8>(try@,";
      fprintf ff "match %s.Jsonrpc.method_name with@," reqvn;
      List.iter (fun (rpc, resp) -> gen_request ff venv reqv implvn cbvn rpc resp) rpcs;
      fprintf ff "| _ -> raise (Jsonrpc.Unknown_request %s.Jsonrpc.method_name)@]@," reqvn;
      let ev, venv = Var_env.new_ident_from_name venv "e" in
      let errv, _venv = Var_env.new_ident_from_name venv "err" in
      let evn, errvn = name_of_var ev, name_of_var errv in
        fprintf ff "@[<v 8> with %s ->@," evn;
        fprintf ff "let %s = %s.%s %s %s in@," errvn implvn server.server_error_handler evn cbvn;
        fprintf ff "Jsonrpc.Error %s)@]@]@," errvn;
        fprintf ff "in@,";
        fprintf ff "Jsonrpc.response_to_json { Jsonrpc.response_id = %s; Jsonrpc.response = %s }@]@,@\n" reqidjvn pvn

  let gen_dispatch ff impl_name =
    fprintf ff "@[<v 8>let dispatch %s req_j callback_arg =@," impl_name;
    fprintf ff "let req = Jsonrpc.request_of_json req_j in@,";
    fprintf ff "match req.Jsonrpc.request_id with@,";
    fprintf ff "| None -> ignore (dispatch_notification %s req callback_arg); None@," impl_name;
    fprintf ff "| Some id -> Some (dispatch_rpc %s id req callback_arg)@]@,@\n" impl_name
end

module Client = struct
  let gen_method_name pn =
    let nm = String.copy pn in
      for i = 0 to (String.length pn) - 1 do
        match pn.[i] with
          | '.' -> nm.[i] <- '_'
          | 'A' .. 'Z' -> nm.[i] <- Char.lowercase pn.[i]
          | _ -> ()
      done;
      nm

  let gen_handler_type ff resp =
    let resp_type = resp.response_value.param_type in
      fprintf ff "@,val %s : %s -> context -> unit" resp.response_handler resp_type

  let client_modname s = Printf.sprintf "%s_client" (String.capitalize s.server_name)

  let gen_resp_name resp =
    Printf.sprintf "jresp_%s" (gen_method_name resp.response_handler)

  let generate_client_sig ff s rpcs =
    fprintf ff "module type %s =@\n" (client_modname s);
    fprintf ff "@[<v 8>sig";
    fprintf ff "@,type rpc_id";
    fprintf ff "@,type context@,";
    fprintf ff "@,val get_new_rpc_id : unit -> rpc_id * Json.t@,";
    ignore (List.fold_left (fun acc (_rpc, resp) ->
                              if List.mem resp.response_handler acc then acc
                              else begin
                                gen_handler_type ff resp;
                                resp.response_handler :: acc
                              end
                           ) [] rpcs);
    fprintf ff "@]@\nend@\n@\n@?"

  let start_maker ff s =
    let modname = client_modname s in
      fprintf ff "module Make_%s_client (%s : %s) =@\n" (String.lowercase s.server_name) modname modname;
      fprintf ff "@[<v 8>struct";
      modname

  let end_maker ff =
    fprintf ff "@]@\nend@\n@\n@?"

  let gen_resp_handler ff modname _c (_rpc, resp) =
    fprintf ff "@,@[<v 8>let %s resp =@," (gen_resp_name resp);
    fprintf ff "%s.%s (%s resp)@]@," modname resp.response_handler
      (Type_conv.of_json resp.response_value.param_type)

  let generate_resp_handlers ff modname c rpc_list =
    ignore (List.fold_left (fun acc (rpc, resp) ->
                              if List.mem resp.response_handler acc then acc
                              else begin
                                gen_resp_handler ff modname c (rpc, resp);
                                resp.response_handler :: acc
                              end
                           ) [] rpc_list)

  let generate_rpc ff venv modname _s rpc =
    let params = rpc.rpc_request.request_params in
    let args = List.map (fun p -> p.param_name) params in
    let avlist, venv = Var_env.new_idents_from_names venv ~prefix:"o_" args in
    let vvlist, venv = Var_env.new_idents_from_names venv ~prefix:"j_" args in
    let rpcv, venv = Var_env.new_ident_from_name venv "rpc_id" in
    let jrpcv, _venv = Var_env.new_ident_from_name venv "jrpc_id" in
    let rpcvn, jrpcvn = name_of_var rpcv, name_of_var jrpcv in
    let meth_name = gen_method_name rpc.rpc_request.request_name in
    let args_str =
      (match args with
         | [] ->
             fprintf ff "@,@[<v 8>let jrpc_%s () =@," meth_name;
             ""
         | _  ->
             fprintf ff "@,@[<v 8>let jrpc_%s %s =@," meth_name (String.concat " " (List.map name_of_var avlist));
             List.iter2 (fun p (a, v) ->
                           fprintf ff "let %s = %s %s in@,"
                             (name_of_var v) (Type_conv.to_json p.param_type) (name_of_var a)
                        ) params (List.combine avlist vvlist);
             String.concat "; " (List.map2 (fun a v ->
                                              Printf.sprintf "\"%s\", %s" a (name_of_var v)
                                           ) args vvlist
                                )
      ) in
      (match rpc.rpc_response with
         | None ->
             fprintf ff "let %s = None in@," jrpcvn
         | Some _resp ->
             fprintf ff "let %s, %s = %s.get_new_rpc_id () in@," rpcvn jrpcvn modname;
             fprintf ff "let %s = Some %s in@," jrpcvn jrpcvn
      );
      fprintf ff "@[<v 2>{ Jsonrpc.request_id = %s;@," jrpcvn;
      fprintf ff "Jsonrpc.method_name = \"%s\";@," rpc.rpc_request.request_name;
      fprintf ff "Jsonrpc.params = Json.Object (Array.of_list [ %s ])" args_str;
      (match rpc.rpc_response with
         | None -> fprintf ff "@]@,}@]"
         | Some resp -> fprintf ff "@]@,}, %s, %s@]@," rpcvn (gen_resp_name resp)
      )
end

let generate_header ff =
  let argv = Array.to_list Sys.argv in
  let argv = (Filename.basename (List.hd argv)) :: (List.tl argv) in
  let call = String.concat " " argv in
    fprintf ff "(* This file has been auto-generated using \"%s\". *)@\n@\n" call

let generate_opens ff spec =
  List.iter (fun m -> fprintf ff "open %s@\n" (String.capitalize m)) (get_uses spec);
  fprintf ff "@\n"

let open_output fn =
  (try Unix.unlink fn with _ -> ());
  let op_flags = [ Open_wronly ; Open_creat; Open_trunc; Open_text ] in
  let oc = open_out_gen op_flags 0o444 fn in
  let ff = formatter_of_out_channel oc in
    oc, ff

let generate_server ff spec s =
  Server.start_server ff s;
  let rpc_list, notif_list = get_sorted_rpcs_by_server spec s in
  let sig_name = Server.gen_dispatch_struct ff s rpc_list notif_list in
    Server.gen_messages_handled_list ff rpc_list notif_list;
    Server.gen_rpc_dispatch ff Var_env.new_env s sig_name rpc_list;
    Server.gen_notification_dispatch ff Var_env.new_env s sig_name notif_list;
    Server.gen_dispatch ff sig_name;
    Server.end_server ff;
    fprintf ff "@\n@?"

let generate_client ff spec c =
  let rpc_list, _ = get_sorted_rpcs_by_server spec c in
  let msg_list = get_rpcs_by_server spec c in
    Client.generate_client_sig ff c rpc_list;
    let modname = Client.start_maker ff c in
      if List.length rpc_list > 0 then
        fprintf ff "@,(* Response handling *)@,";
      Client.generate_resp_handlers ff modname c rpc_list;
      fprintf ff "@,";
      if List.length msg_list > 0 then
        fprintf ff "@,(* Requests and notifications *)@,";
      List.iter (Client.generate_rpc ff Var_env.new_env modname c) msg_list;
      Client.end_maker ff

let generate_endpoint spec e fn =
  let oc, ff = open_output fn in
    generate_header ff;
    generate_opens ff spec;
    List.iter (fun s ->
                 generate_server ff spec s
              ) (get_matching_servers spec e.endpoint_servers);
    List.iter (fun c ->
                 generate_client ff spec c
              ) (get_matching_servers spec e.endpoint_clients);
    close_out oc

let generate spec endpoints =
  List.iter (fun (ep, outf) ->
               generate_endpoint spec ep outf
            ) endpoints
