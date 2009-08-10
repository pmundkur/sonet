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

open Rpc_types
open Rpc_types_json_conv
open Rpc_one
open Rpc_two

exception Dumb_test_error

(* First, implement the response handling interface. *)
module I = struct
  type rpc_id = Int64.t

  let cur_id = ref 0L
  let get_new_rpc_id () =
    let id = !cur_id in
      cur_id := Int64.add !cur_id 1L;
      id, Json.Int id

  type context =
    | Req0 of resp1_type
    | Req1 of resp1_type
    | Req2 of resp2_type

  let response0_handler resp1 context =
    match context with
      | Req0 r -> assert (r = resp1); Printf.printf "response0_handler: response good\n\n"
      | _ -> raise Dumb_test_error

  let response1_handler resp2 context =
    match context with
      | Req1 r -> assert (r = resp2);  Printf.printf "response1_handler: response good\n\n"
      | _ -> raise Dumb_test_error

  let client_function resp2 context =
    match context with
      | Req2 r -> assert (r = resp2);  Printf.printf "client_function: response good\n\n"
      | _ -> raise Dumb_test_error
end

(* Now, create the client-side wrappers. *)

module C = Make_server_client (I)

(* Finally, implement the server-side call dispatch structure. *)
module S = struct
  let req0_handler () () = Some true

  let req1_handler arg1 () =
    if arg1 < 5 then None
    else if arg1 < 10 then Some false
    else Some true

  let req2_handler arg1 arg2 arg3 () =
    ((string_of_int arg1)
     ^ ".[" ^ (String.concat "," (List.map (fun (s, i) -> s ^ "-" ^ (string_of_int i)) arg2))
     ^ "]." ^ (if arg3 then "true" else "false"))

  let not1_handler arg1 arg2 arg3 () =
    assert ((arg1 = 5)
            && (arg2 = ["5", 5; "10", 10])
            && arg3)

  let error_handler e () =
    2, Printexc.to_string e, Some (Json.String "details")

  let message_filter s () =
    assert ((List.mem s Server.rpcs_handled)
            || (List.mem s Server.notifications_handled))

  let server_impl = {
    Server.request0_handler = req0_handler;
    Server.request1_handler = req1_handler;
    Server.request2_handler = req2_handler;
    Server.not1_handler = not1_handler;
    Server.server_message_filter = message_filter;
    Server.server_error_handler = error_handler;
  }
end

let rpc_invoke req resp_fun =
  (* Client-side request processing: *)

  (* 1) create the corresponding json-rpc request object to send over the wire. *)
  let jreq_c = Jsonrpc.request_to_json req in

  (* 2) send it over the wire. *)
  let jnet_c = Json.to_string jreq_c in
  let _ = Printf.printf "Sending request: %s\n" jnet_c in

  (* Server-side processing: *)

  (* i) parse the string into a json value *)
  let ps = Json_parse.init_parse_state () in
  let jreq_s = match Json_parse.parse ps jnet_c with
    | Json_parse.Json_value (j, _) -> j
    | Json_parse.Json_parse_incomplete _ -> raise (Failure "server json parsing") in

  (* ii) dispatch the request *)
  let resp_j = Server.dispatch S.server_impl jreq_s () in

  (* iii) check whether we have a response to send back *)
  let resp_j = match resp_j with
    | None -> raise (Failure "unexpected notification")
    | Some j -> j in

  (* iv) send it over the wire *)
  let jnet_s = Json.to_string resp_j in
  let _ = Printf.printf "Sending response: %s\n" jnet_s in

  (* Client-side response processing: *)

  (* a) parse the string into a json value *)
  let pc = Json_parse.init_parse_state () in
  let jresp_c = match Json_parse.parse pc jnet_s with
    | Json_parse.Json_value (j, _) -> j
    | Json_parse.Json_parse_incomplete _ -> raise (Failure "client json parsing") in

  (* b) extract the response *)
  let resp = Jsonrpc.response_of_json jresp_c in

    (* c) process that response *)
    resp_fun resp

let notification_invoke testname req =
  (* Client-side request processing: *)

  (* 1) create the corresponding json-rpc request object to send over the wire. *)
  let jreq_c = Jsonrpc.request_to_json req in

  (* 2) send it over the wire. *)
  let jnet_c = Json.to_string jreq_c in
  let _ = Printf.printf "Sending request: %s\n" jnet_c in

  (* Server-side processing: *)

  (* i) parse the string into a json value *)
  let ps = Json_parse.init_parse_state () in
  let jreq_s = match Json_parse.parse ps jnet_c with
    | Json_parse.Json_value (j, _) -> j
    | Json_parse.Json_parse_incomplete _ -> raise (Failure "server json parsing") in

  (* ii) dispatch the request *)
  let resp_j = Server.dispatch S.server_impl jreq_s () in

    (* iii) check whether we have a response to send back *)
    (match resp_j with
       | None -> ()
       | Some j -> raise (Failure (Printf.sprintf "unexpected response in test %s" testname)))


let default_id_check req_id resp_id =
  match req_id with
    | None -> if not (Json.is_null resp_id) then raise (Failure "unexpected non-null resp id received")
    | Some id -> if (id <> resp_id) then raise (Failure "resp id differs from req id")

let default_error_check e =
  raise (Failure "unexpected rpc error received.")

let test_invoke req ?(id_check=default_id_check) ?(error_check=default_error_check) result_check =
  let resp_fun resp =
    id_check req.Jsonrpc.request_id resp.Jsonrpc.response_id;
    match resp.Jsonrpc.response with
      | Jsonrpc.Result r -> result_check r
      | Jsonrpc.Error e -> error_check e
  in
    rpc_invoke req resp_fun

let test_server () =
  let req0_checker test_id () =
    let req, rpcid, resp_fn = C.jrpc_request0 () in
    let exp_resp = S.req0_handler () () in
    let resp_to_str r = match r with |None -> "None" | Some b -> if b then "Some true" else "Some false" in
    let resp_checker r =
      let got_resp = resp1_type_of_json r in
        if got_resp <> exp_resp
        then raise (Failure (Printf.sprintf "req1, test %s: got \"%s\", expected \"%s\"!"
                               test_id (resp_to_str got_resp) (resp_to_str exp_resp)));
        resp_fn r (I.Req0 exp_resp)
    in
      test_invoke req resp_checker
  in


  let req1_checker test_id arg1 =
    let req, rpcid, resp_fn = C.jrpc_request1 arg1 in
    let exp_resp = S.req1_handler arg1 () in
    let resp_to_str r = match r with |None -> "None" | Some b -> if b then "Some true" else "Some false" in
    let resp_checker r =
      let got_resp = resp1_type_of_json r in
        if got_resp <> exp_resp
        then raise (Failure (Printf.sprintf "req1, test %s: got \"%s\", expected \"%s\"!"
                               test_id (resp_to_str got_resp) (resp_to_str exp_resp)));
        resp_fn r (I.Req1 exp_resp)
    in
      test_invoke req resp_checker
  in

  let req2_checker test_id arg1 arg2 arg3 =
    let req, rpcid, resp_fn = C.jrpc_request2 arg1 arg2 arg3 in
    let exp_resp = S.req2_handler arg1 arg2 arg3 () in
    let resp_checker r =
      let got_resp = resp2_type_of_json r in
        if got_resp <> exp_resp
        then raise (Failure (Printf.sprintf "req2, test %s: got \"%s\", expected \"%s\"!" test_id got_resp exp_resp));
        resp_fn r (I.Req2 exp_resp)
    in
      test_invoke req resp_checker
  in

  let notif = C.jrpc_notification1 5 ["5", 5; "10", 10] true
  in

    (* Tests *)
    req0_checker "1" ();
    req1_checker "1" 3;
    req1_checker "2" 7;
    req1_checker "3" 13;

    req2_checker "1" 1 [] true;
    req2_checker "2" 2 [("2", 2)] true;
    req2_checker "3" 5 [("2", 2); ("9", 6)] false;


    notification_invoke "not1: case 1" notif

let _ =
  test_server ()
