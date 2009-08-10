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

val code_error_parse : int
val code_error_invalid_req : int
val code_error_not_found : int
val code_error_invalid_params : int
val code_error_internal_error : int

type req_error =
  | Request_json_conv_error of Json_conv.error
  | Request_invalid_params
val string_of_req_error: req_error -> string

type resp_error =
  | Response_json_conv_error of Json_conv.error
  | Response_no_result_or_error
  | Response_both_result_and_error
val string_of_resp_error: resp_error -> string

exception Invalid_id of Json.t
exception Invalid_request of req_error
exception Invalid_response of resp_error
exception Unknown_request of string

type rpc_request = {
  request_id : Json.t option;
  method_name : string;
  params : Json.t;
}

type rpc_error = int * string * Json.t option (* code, message, optional_data *)
type rpc_response_payload =
  | Result of Json.t
  | Error of rpc_error

type rpc_response = {
  response_id : Json.t;
  response : rpc_response_payload;
}

(** create a rpc response error message with id -> code -> message -> data option *)
val response_make_error : Json.t -> int -> string -> Json.t option -> rpc_response

(** create a rpc response success with id -> data *)
val response_make_success : Json.t -> Json.t -> rpc_response

val request_of_json: Json.t -> rpc_request
val request_to_json: rpc_request -> Json.t
val response_of_json: Json.t -> rpc_response
val response_to_json: rpc_response -> Json.t

val request_of_string: string -> rpc_request
val request_to_string: rpc_request -> string
val response_of_string: string -> rpc_response
val response_to_string: rpc_response -> string

(* determine what type of jsonrpc object a json value is *)
type rpc_type =
  | Non_JSONRPC
  | Request of (* id *) Json.t * rpc_request
  | Notification of rpc_request
  | Response of rpc_response

val rpc_type_of_json: Json.t -> rpc_type
val is_jsonrpc_value: Json.t -> bool
