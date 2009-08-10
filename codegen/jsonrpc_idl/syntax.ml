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

type use = {
  use_modules: string list;
}

type server = {
  server_name: string;
  server_doc: string;
  server_message_filter: string option;
  server_error_handler: string;
}

type param = {
  param_name: string;
  param_doc:  string;
  param_type:  string;
}

type request = {
  request_name: string;
  request_doc: string;
  request_handler: string;
  request_params: param list;
}

type response = {
  response_doc: string;
  response_handler: string;
  response_value: param;
}

type rpc = {
  rpc_type: string;
  rpc_server: string;
  rpc_doc: string;
  rpc_version: string;

  rpc_deprecated: string option;
  rpc_label_arguments: bool option;

  rpc_request: request;
  rpc_response: response option;
}

type endpoint = {
  endpoint_name: string;
  endpoint_servers: string list;
  endpoint_clients: string list;
}
