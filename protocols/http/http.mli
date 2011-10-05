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

type header_fields = (string * string list) list

val add_header : string -> string -> header_fields -> header_fields
val is_header_present : string -> header_fields -> bool
val lookup_header : string -> header_fields -> string list (* can throw Not_found *)

(* Protocol constants *)

type version = HTTP09 | HTTP10 | HTTP11

val string_of_version : version -> string

type meth =
  | Get
  | Put
  | Head
  | Post
  | Trace
  | Delete
  | Connect
  | Options
  | Extension of string

val string_of_meth : meth -> string

type status =
    (* Informational 1xx *)
  | Status_continue
  | Status_switching_protocols

    (* Successful 2xx *)
  | Status_ok
  | Status_created
  | Status_accepted
  | Status_non_authoritative
  | Status_no_content
  | Status_reset_content
  | Status_partial_content

    (* Redirection 3xx *)
  | Status_multiple_choices
  | Status_moved_permanently
  | Status_found
  | Status_see_other
  | Status_not_modified
  | Status_use_proxy
  | Status_temporary_redirect

    (* Client Error 4xx *)
  | Status_bad_request
  | Status_unauthorized
  | Status_payment_required
  | Status_forbidden
  | Status_not_found
  | Status_method_not_allowed
  | Status_not_acceptable
  | Status_proxy_authentication_required
  | Status_request_timeout
  | Status_conflict
  | Status_gone
  | Status_length_required
  | Status_precondition_failed
  | Status_request_entity_too_large
  | Status_request_uri_too_large
  | Status_unsupported_media_type
  | Status_requested_range_not_satisfiable
  | Status_expectation_failed

    (* Server Error 5xx *)
  | Status_internal_server_error
  | Status_not_implemented
  | Status_bad_gateway
  | Status_service_unavailable
  | Status_gateway_timeout
  | Status_version_not_supported

    (* Other *)
  | Status_other of int * string

val status_info : status -> (* status code *) int * (* reason phrase *) string

module Headers : sig
  type error
  exception Http_error of error
  val string_of_error : error -> string

  val serialize : Buffer.t -> header_fields -> unit
end

module Request_header : sig
  type url =
    | Star
    | Uri of Uri.t
  val string_of_url : url -> string

  type state
  val init_state : unit -> state
  val num_bytes_parsed : state -> int

  type t = {
    version : version;
    meth : meth;
    url : url;
    headers : header_fields;
  }
  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
  val parse : state -> string -> parse_result
  val parse_substring : state -> string -> (* offset *) int -> (* len *) int -> parse_result

  type error
  exception Http_error of error
  val string_of_error : error -> string

  val serialize : Buffer.t -> t -> unit
end

module Response_header : sig
  type state
  val init_state : unit -> state
  val num_bytes_parsed : state -> int

  type t = {
    version : version;
    status_code : int;
    reason_phrase : string;
    headers : header_fields;
  }
  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
  val parse : state -> string -> parse_result
  val parse_substring : state -> string -> (* offset *) int -> (* len *) int -> parse_result

  type error
  exception Http_error of error
  val string_of_error : error -> string

  val serialize : Buffer.t -> t -> unit
end

type payload_callback = string -> (* offset *) int -> (* length *) int -> (* final *) bool -> unit

module Payload : sig
  type state
  val num_bytes_parsed : state -> int64

  type payload_type =
    | No_payload
    | Payload of state
    | Error of string

  val init_from_response:
    ?payload_callback:payload_callback -> ?max_payload_length:int64
    -> Response_header.t -> payload_type
  val init_from_request:
    ?payload_callback:payload_callback -> ?max_payload_length:int64
    -> Request_header.t -> payload_type

  type error
  val string_of_error : error -> string
  exception Http_error of error

  type t = {
    content : Buffer.t;
    trailers : header_fields
  }
  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
  val parse : state -> string -> parse_result
  val parse_substring : state -> string -> (* offset *) int -> (* len *) int -> parse_result
  val get_parse_result : state -> t option

  val connection_closed : state -> unit

  (* Note: this is only recommended for small payloads! *)
  val serialize_of_request : Request_header.t -> Buffer.t -> t -> unit
  val serialize_of_response : Response_header.t -> Buffer.t -> t -> unit
end

module Request : sig
  type state
  type header_callback = Request_header.t -> Request_header.t

  val init_state : ?header_callback:header_callback -> ?payload_callback:payload_callback -> unit -> state
  val num_bytes_parsed : state -> int64

  type error
  val string_of_error : error -> string
  exception Http_error of error

  type t = {
    request : Request_header.t;
    payload : Payload.t option
  }
  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
    | Error of string
  val parse : state -> string -> parse_result
  val parse_substring : state -> string -> (* offset *) int -> (* len *) int -> parse_result
  val get_parse_result : state -> t option

  val connection_closed : state -> unit

  val serialize : Buffer.t -> t -> unit

  (* utility accessors *)
  val version : t -> version
  val meth : t -> meth
  val headers : t -> header_fields
  val payload_buf : t -> Buffer.t option
end

module Response : sig
  type state
  type header_callback = Response_header.t -> Response_header.t

  val init_state : ?header_callback:header_callback -> ?payload_callback:payload_callback -> Request_header.t -> state
  val num_bytes_parsed : state -> int64

  type error = Internal_error of string
  val string_of_error : error -> string
  exception Http_error of error

  type t = {
    response : Response_header.t;
    payload : Payload.t option
  }
  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
    | Error of string
  val parse : state -> string -> parse_result
  val parse_substring : state -> string -> (* offset *) int -> (* len *) int -> parse_result
  val get_parse_result : state -> t option

  val connection_closed : state -> unit

  val serialize : Buffer.t -> t -> unit

  val make_response : ?payload:Payload.t -> headers:header_fields -> status -> t

  (* utility accessors *)
  val status_code : t -> int
  val headers : t -> header_fields
  val payload_buf : t -> Buffer.t option
end
