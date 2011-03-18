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

open Json_conv

let code_error_parse = -32700           (* Invalid JSON. An error occurred on the server while parsing the JSON text. *)
let code_error_invalid_req = -32600     (* The received JSON not a valid JSON-RPC Request. *)
let code_error_not_found = -32601       (* The requested remote-procedure does not exist / is not available. *)
let code_error_invalid_params = -32602  (* Invalid method parameters. *)
let code_error_internal_error = -32603  (* Internal JSON-RPC error. *)

type req_error =
  | Request_json_conv_error of error
  | Request_invalid_params

let string_of_req_error = function
  | Request_json_conv_error e ->
      Printf.sprintf "jsonrpc type-conversion error in request: %s"
        (string_of_error e)
  | Request_invalid_params ->
      "jsonrpc error: invalid request params"

type resp_error =
  | Response_json_conv_error of error
  | Response_no_result_or_error
  | Response_both_result_and_error

let string_of_resp_error = function
  | Response_json_conv_error e ->
      Printf.sprintf "jsonrpc type-conversion error in response: %s"
        (string_of_error e)
  | Response_no_result_or_error ->
      "jsonrpc error: invalid response (no result or error)"
  | Response_both_result_and_error ->
      "jsonrpc error: invalid response (both result and error)"

exception Invalid_id of Json.t
exception Invalid_request of req_error
exception Invalid_response of resp_error
exception Unknown_request of string

type rpc_request = {
  request_id: Json.t option;
  method_name: string;
  params: Json.t
}

type rpc_error = int * string * Json.t option (* code, message, optional_data *)
type rpc_response_payload =
  | Result of Json.t
  | Error of rpc_error

type rpc_response = {
  response_id: Json.t;
  response: rpc_response_payload;
}

let request_of_json j =
  try
    let obj = to_object_table j in
    let id = optional_object_field obj "id" in
    let id = if id = Json.Null then None else Some id in
    let method_name = to_string (object_field obj "method") in
    let params = object_field obj "params" in
      if not (Json.is_object params) && not (Json.is_array params) then
        raise (Invalid_request Request_invalid_params);
      { request_id = id; method_name = method_name; params = params }
  with Json_conv_error e -> raise (Invalid_request (Request_json_conv_error e))

let request_of_string s = request_of_json (Json_parse.of_string s)

let request_to_json req =
  let fields = (("jsonrpc", of_string "2.0")
                :: ("method", of_string req.method_name)
                :: ("params", req.params)
                :: (match req.request_id with
                      | None -> []
                      | Some id -> [ ("id", id) ])) in
    Json.Object (Array.of_list fields)

let request_to_string req = Json.to_string (request_to_json req)

let response_make_error id code message data =
  { response_id = id; response = Error (code, message, data) }

let response_make_success id data =
  { response_id = id; response = Result data }

let response_of_json j =
  let error_of_json j =
    let obj = to_object_table j in
    let code = to_int (object_field obj "code") in
    let message = to_string (object_field obj "message") in
    let data = optional_object_field obj "data" in
      code, message, (match data with Json.Null -> None | j -> Some j)
  in
    try
      let obj = to_object_table j in
      let id = object_field obj "id" in
      let result = optional_object_field obj "result" in
      let error =  optional_object_field obj "error" in
        match result, error with
          | _, Json.Null ->
              response_make_success id result
          | Json.Null, _ ->
              let c, m, d = error_of_json error in
                response_make_error id c m d
          | _ ->
              raise (Invalid_response Response_both_result_and_error)
    with Json_conv_error e -> raise (Invalid_response (Response_json_conv_error e))

let response_of_string s = response_of_json (Json_parse.of_string s)

let response_to_json resp =
  let result_to_json id result =
    Json.Object [| ("jsonrpc", of_string "2.0");
                   ("id", id);
                   ("result", result)
                |]
  in
  let error_to_json id (c, m, d) =
    let err = (("code", of_int c)
               :: ("message", of_string m)
               :: (match d with None -> [] | Some d -> [ ("data", d) ])) in
      Json.Object [| ("jsonrpc", of_string "2.0");
                     ("id", id);
                     ("error", Json.Object (Array.of_list err))
                  |]
  in
    match resp.response with
      | Result r -> result_to_json resp.response_id r
      | Error e  -> error_to_json resp.response_id e

let response_to_string resp = Json.to_string (response_to_json resp)


type rpc_type =
  | Non_JSONRPC
  | Request of (* id *) Json.t * rpc_request
  | Notification of rpc_request
  | Response of rpc_response

let rpc_type_of_json j =
  (* First try request/notification. *)
  try
    let req = request_of_json j in
      match req.request_id with
        | None -> Notification req
        | Some id -> Request (id, req)
  with _ ->
    (* Next, try a response. *)
    try
      Response (response_of_json j)
    with _ ->
      Non_JSONRPC

let is_jsonrpc_value j =
  rpc_type_of_json j <> Non_JSONRPC
