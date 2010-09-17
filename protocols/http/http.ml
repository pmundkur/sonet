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

let verbose = ref false

let dbg fmt =
  let logger s = if !verbose then Printf.printf "%s\n%!" s in
    Printf.ksprintf logger fmt

let optval = function | Some v -> v | None -> assert false

let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

let rev_string_of_chars cl =
  let len = List.length cl in
  let s = String.create len in
    ignore (List.fold_left (fun idx c -> s.[idx] <- c; idx - 1) (len - 1) cl);
    s

let is_ctl_char c =
  (* Note that this considers '\t', '\n' and '\r' as control chars. *)
  let code = Char.code c in
    code < 32 || code > 127

(* characters in a token include all 7-bit chars except CTLs (0-31),
   DEL(127), and the following separators: "(", ")", "<", ">", "@",
   ",", ";", ":", "\", <">, "/", "[", "]", "?", "=", "{", "}", SP, HT
*)
let is_token_char_array = [|
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 1; 0; 1; 1; 1; 1; 1; 0; 0; 1; 1; 0; 1; 1; 0;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0;
  0; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 1; 0; 1; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
|]

let is_token_char c =
  is_token_char_array.(Char.code c) = 1

let rec strip_leading_spaces = function
  | ' ' :: clist | '\t' :: clist ->
      strip_leading_spaces clist
  | cl -> cl

(* Header utilities *)

type header_fields = (string * string list) list

let rec add_header fname fvalue =
  let fname = String.lowercase fname in
  let rec adder = function
    | [] -> [ fname, [fvalue] ]
    | (n, vl) :: hdrs when n = fname -> (n, fvalue :: vl) :: hdrs
    | (n, vl) :: hdrs -> (n, vl) :: (adder hdrs)
  in adder

let lookup_header name hdrs =
  List.assoc (String.lowercase name) hdrs

let is_header_present hdr headers =
  try ignore (lookup_header hdr headers); true
  with Not_found -> false

type version =
  | HTTP09
  | HTTP10
  | HTTP11

let string_of_version = function
  | HTTP09 -> ""
  | HTTP10 -> "HTTP/1.0"
  | HTTP11 -> "HTTP/1.1"

(* TODO: impose a max length on field names and values *)

module Headers = struct
  type cursor =
    | Start_header
    | In_field_name of char list
    | In_field_value of string * char list * (* starts with whitespace *) bool
    | In_field_value_CR of string * char list * (* starts with whitespace *) bool
    | In_field_value_LF of string * char list * (* starts with whitespace *) bool
    | Header_CR
    | Done

  let string_of_cursor = function
    | Start_header -> "Start-header"
    | In_field_name cl ->
        Printf.sprintf "In-field-name \"%s\"" (rev_string_of_chars cl)
    | In_field_value (s, cl, _) ->
        Printf.sprintf "In-field-value \"%s: %s\"" s (rev_string_of_chars cl)
    | In_field_value_CR (s, cl, _) ->
        Printf.sprintf "In-field-value-CR \"%s: %s\"" s (rev_string_of_chars cl)
    | In_field_value_LF (s, cl, _) ->
        Printf.sprintf "In-field-value-LF \"%s: %s\"" s (rev_string_of_chars cl)
    | Header_CR -> "Header-CR"
    | Done -> "Done"

  type error =
    | Parse_error of cursor * char
    | Internal_error of string

  let string_of_error = function
    | Parse_error (cursor, c) ->
        Printf.sprintf "Header parse error in state %s at char '%C'"
          (string_of_cursor cursor) c
    | Internal_error s ->
        Printf.sprintf "Internal header-parsing error: %s" s

  exception Http_error of error

  let raise_error err = raise (Http_error err)

  type state = {
    mutable cursor: cursor;
    mutable headers: header_fields;
  }

  let init_state () = {
    cursor = Start_header;
    headers = [];
  }

  let is_done s = s.cursor = Done

  let raise_bad_char s c = raise_error (Parse_error (s.cursor, c))
  let parse_char s c =
    (* dbg "parsing %C in state %s..." c (string_of_cursor s.cursor); *)
      match s.cursor with
        | Start_header ->
            (match c with
               | '\r' -> s.cursor <- Header_CR
               | '\n' -> s.cursor <- Done
               | _    -> s.cursor <- In_field_name [ c ]
            )
        | Header_CR ->
            if c = '\n' then s.cursor <- Done
            else raise_bad_char s c
        | In_field_name clist ->
            (* Ideally, we want to remain in the
               In_field_name state only if we are getting
               characters that are legal for a HTTP token,
               some web servers don't follow the standard.
               We once encountered one that used to give a
               "Content Location:", i.e., it had an
               invalid SP character in the field name
               token.  To get around this, we allow any
               characters in the token upto the first ':'
               (except '\r' and '\n').
            *)
            (match c with
               | '\r' | '\n' -> raise_bad_char s c
               | ':' -> s.cursor <- In_field_value ((rev_string_of_chars clist), [], true)
               | _   -> s.cursor <- In_field_name (c :: clist)
            )
        | In_field_value (name, clist, starts_with_space) ->
            (match c with
               | '\r' -> s.cursor <- In_field_value_CR (name, clist, starts_with_space)
               | '\n' -> s.cursor <- In_field_value_LF (name, clist, starts_with_space)
               | ' ' | '\t' ->
                   (* compress whitespace *)
                   let nclist = if starts_with_space then clist else c :: clist in
                     s.cursor <- In_field_value (name, nclist, true)
               | _ -> s.cursor <- In_field_value (name, c :: clist, false)
            )
        | In_field_value_CR (name, clist, starts_with_space) ->
            if c = '\n' then s.cursor <- In_field_value_LF (name, clist, starts_with_space)
            else raise_bad_char s c
        | In_field_value_LF (name, clist, starts_with_space) ->
            (match c with
               | ' ' | '\t' ->
                   (* header has line-wrapped; compress whitespace *)
                   let nclist = if starts_with_space then clist else c :: clist in
                     s.cursor <- In_field_value (name, nclist, true)
               | _ ->
                   (* end-of-header line; strip trailing whitespace if any *)
                   s.headers <- (add_header name
                                   (rev_string_of_chars (strip_leading_spaces clist))
                                   s.headers);
                   (match c with
                      | '\r' -> s.cursor <- Header_CR
                      | '\n' -> s.cursor <- Done
                      | _    -> s.cursor <- In_field_name [ c ]
                   )
            )
        | Done -> raise_error (Internal_error "parse called on finished request!")

  let serialize buf hdrs =
    let hdr (fn, fvl) =
      List.iter (fun fv ->
                   Buffer.add_string buf fn;
                   Buffer.add_string buf ": ";
                   Buffer.add_string buf fv;
                   Buffer.add_string buf "\r\n")
        (List.rev fvl)
    in
      List.iter hdr (List.rev hdrs);
      Buffer.add_string buf "\r\n"
end


module Request_header = struct
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

  let string_of_meth = function
    | Get           -> "GET"
    | Put           -> "PUT"
    | Head          -> "HEAD"
    | Post          -> "POST"
    | Trace         -> "TRACE"
    | Delete        -> "DELETE"
    | Connect       -> "CONNECT"
    | Options       -> "OPTIONS"
    | Extension m   -> m

  type url =
    | Star
    | Uri of Uri.t

  let string_of_url = function
    | Star  -> "*"
    | Uri u -> Uri.to_string (Uri.normalize u)

  type cursor =
    | Start
    | In_method of char list
    | Method_SP
    | In_uri of char list
    | Uri_SP
    | Uri_CR
    | In_version of char list
    | Req_line_CR
    | Req_line_SP
    | In_headers of Headers.state
    | Done

  let string_of_cursor = function
    | Start -> "Start"
    | In_method cl ->
        Printf.sprintf "In-method \"%s\"" (rev_string_of_chars cl)
    | Method_SP -> "Method-SP"
    | In_uri cl ->
        Printf.sprintf "In-uri \"%s\"" (rev_string_of_chars cl)
    | Uri_CR -> "Uri-CR"
    | Uri_SP -> "Uri-SP"
    | In_version cl ->
        Printf.sprintf "In-version \"%s\"" (rev_string_of_chars cl)
    | Req_line_CR -> "Req-line-CR"
    | Req_line_SP -> "Req-line-SP"
    | In_headers hs ->
        Printf.sprintf "In-headers (%s)" (Headers.string_of_cursor hs.Headers.cursor)
    | Done -> "Done"

  type state = {
    mutable cursor: cursor;
    mutable s_meth: meth option;
    mutable s_url: url option;
    mutable s_version: version option;
    mutable s_headers: header_fields;
    mutable num_bytes_parsed: int
  }

  type error =
    | Unsupported_version of string
    | Incomplete_request_line
    | Parse_error of cursor * char
    | Internal_error of string
    | Invalid_request_uri of Uri.error

  let string_of_error = function
    | Unsupported_version s ->
        Printf.sprintf "Unsupported version in request: \"%s\"" s
    | Incomplete_request_line ->
        Printf.sprintf "Incomplete request line"
    | Parse_error (cursor, c) ->
        Printf.sprintf "Request parse error in state %s at char '%C'"
          (string_of_cursor cursor) c
    | Internal_error s ->
        Printf.sprintf "Internal request-parsing error: %s" s
    | Invalid_request_uri e ->
        Printf.sprintf "Invalid request uri: %s" (Uri.string_of_error e)

  exception Http_error of error

  let raise_error err = raise (Http_error err)

  let init_state () = {
    cursor = Start;
    s_meth = None;
    s_url = None;
    s_version = Some HTTP09;
    s_headers = [];
    num_bytes_parsed = 0
  }

  let num_bytes_parsed state =
    if state.cursor = Start then 0 else state.num_bytes_parsed

  (* From http://tools.ietf.org/html/rfc1945#section-4:
     Simple-Request  = "GET" SP Request-URI CRLF                  ; HTTP/0.9
     Request-Line    = Method SP Request-URI SP HTTP-Version CRLF ; HTTP/{1.0,1.1}
  *)

  let raise_bad_char s c = raise_error (Parse_error (s.cursor, c))
  let parse_char s c =
    (* dbg "parsing %C in state %s..." c (string_of_cursor s.cursor); *)
      match s.cursor with
        | Start ->
            if is_token_char c then s.cursor <- In_method [ c ]
            else if not (is_space c) then raise_bad_char s c
        | In_method cl ->
            (match c with
               | '\r' | '\n' -> raise_error Incomplete_request_line
               | ' ' | '\t' ->
                   let meth = match rev_string_of_chars cl with
                     | "GET"     -> Get
                     | "PUT"     -> Put
                     | "HEAD"    -> Head
                     | "POST"    -> Post
                     | "TRACE"   -> Trace
                     | "DELETE"  -> Delete
                     | "CONNECT" -> Connect
                     | "OPTIONS" -> Options
                     | m         -> Extension m
                   in
                     s.s_meth <- Some meth;
                     s.cursor <- Method_SP
               | _ ->
                   s.cursor <- In_method (c :: cl)
            )
        | Method_SP ->
            (match c with
               | '\r' | '\n' -> raise_error Incomplete_request_line
               | ' '  | '\t' -> ()
               | _ -> s.cursor <- In_uri [ c ]
            )
        | In_uri cl ->
            if is_space c then begin
              let u = rev_string_of_chars cl in
              let url = (match u with
                           | "*" -> Star
                           | _   -> (try Uri (Uri.of_string u)
                                     with Uri.Uri_error e -> raise_error (Invalid_request_uri e))
                        ) in
                s.s_url <- Some url;
                (match c with
                   | ' ' | '\t' -> s.cursor <- Uri_SP
                   | '\r'       -> s.cursor <- Uri_CR
                   | _ (* \n *) -> s.cursor <- Done (* HTTP 0.9 *)
                )
            end else
              s.cursor <- In_uri (c :: cl)
        | Uri_CR ->
            if c = '\n' then s.cursor <- Done (* HTTP 0.9 *)
            else raise_bad_char s c
        | Uri_SP ->
            (match c with
               | ' ' | '\t' -> ()
               | '\r' -> s.cursor <- Uri_CR
               | '\n' -> s.cursor <- Done (* HTTP 0.9 *)
               | _    -> s.cursor <- In_version [ c ]
            )
        | In_version cl ->
            if is_space c then begin
              (match rev_string_of_chars cl with
                 | "HTTP/1.0" -> s.s_version <- Some HTTP10
                 | "HTTP/1.1" -> s.s_version <- Some HTTP11
                 | v -> raise_error (Unsupported_version v)
              );
              (match c with
                 | ' ' | '\t' -> s.cursor <- Req_line_SP
                 | '\r'       -> s.cursor <- Req_line_CR
                 | _ (* \n *) -> s.cursor <- In_headers (Headers.init_state ())
              )
            end else if List.length cl > 8 then
              raise_error (Unsupported_version (rev_string_of_chars cl))
            else
              s.cursor <- In_version (c :: cl)
        | Req_line_SP ->
            (match c with
               | ' ' | '\t' -> ()
               | '\r'       -> s.cursor <- Req_line_CR
               | '\n'       -> s.cursor <- In_headers (Headers.init_state ())
               | _          -> raise_bad_char s c
            )
        | Req_line_CR ->
            if c = '\n' then s.cursor <- In_headers (Headers.init_state ())
            else raise_bad_char s c
        | In_headers hs ->
            Headers.parse_char hs c;
            if Headers.is_done hs then begin
              s.s_headers <- List.rev hs.Headers.headers;
              s.cursor <- Done
            end
        | Done -> raise_error (Internal_error "parse called on finished request!")

  type t = {
    version: version;
    meth: meth;
    url: url;
    headers: header_fields;
  }

  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state

  let get_parse_result state =
    match state.cursor with
      | Done -> Some { version = optval state.s_version;
                       meth = optval state.s_meth;
                       url = optval state.s_url;
                       headers = state.s_headers
                     }
      | _ -> None

  let parse_substring state str ofs len =
    let i = ref ofs in
    let iend = ofs + len in
      while get_parse_result state = None && !i < iend do
        parse_char state str.[!i];
        incr i;
        state.num_bytes_parsed <- state.num_bytes_parsed + 1
      done;
      match get_parse_result state with
        | Some v -> Result (v, !i - ofs)
        | None -> Parse_incomplete state

  let parse state str =
    parse_substring state str 0 (String.length str)

  let serialize buf req =
    Buffer.add_string buf (string_of_meth req.meth);
    Buffer.add_string buf " ";
    Buffer.add_string buf (string_of_url req.url);
    if req.version <> HTTP09 then begin
      Buffer.add_string buf " ";
      Buffer.add_string buf (string_of_version req.version);
    end;
    Buffer.add_string buf "\r\n";
    Headers.serialize buf req.headers
end


module Response_header = struct
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

  let status_info = function
      (* Informational 1xx *)
    | Status_continue            -> 100, "Continue"
    | Status_switching_protocols -> 101, "Switching Protocols"

    (* Successful 2xx *)
    | Status_ok                  -> 200, "Ok"
    | Status_created             -> 201, "Created"
    | Status_accepted            -> 202, "Accepted"
    | Status_non_authoritative   -> 203, "Non-Authoritative Information"
    | Status_no_content          -> 204, "No Content"
    | Status_reset_content       -> 205, "Reset Content"
    | Status_partial_content     -> 206, "Partial Content"

    (* Redirection 3xx *)
    | Status_multiple_choices    -> 300, "Multiple Choices"
    | Status_moved_permanently   -> 301, "Moved Permanently"
    | Status_found               -> 302, "Found"
    | Status_see_other           -> 303, "See Other"
    | Status_not_modified        -> 304, "Not Modified"
    | Status_use_proxy           -> 305, "Use Proxy"
    | Status_temporary_redirect  -> 307, "Temporary Redirect"

    (* Client Error 4xx *)
    | Status_bad_request                         -> 400, "Bad Request"
    | Status_unauthorized                        -> 401, "Unauthorized"
    | Status_payment_required                    -> 402, "Payment Required"
    | Status_forbidden                           -> 403, "Forbidden"
    | Status_not_found                           -> 404, "Not Found"
    | Status_method_not_allowed                  -> 405, "Method Not Allowed"
    | Status_not_acceptable                      -> 406, "Not Acceptable"
    | Status_proxy_authentication_required       -> 407, "Proxy Authentication Required"
    | Status_request_timeout                     -> 408, "Request Timeout"
    | Status_conflict                            -> 409, "Conflict"
    | Status_gone                                -> 410, "Gone"
    | Status_length_required                     -> 411, "Length Required"
    | Status_precondition_failed                 -> 412, "Precondition Failed"
    | Status_request_entity_too_large            -> 413, "Request Entity Too Large"
    | Status_request_uri_too_large               -> 414, "Request-URI Too Large"
    | Status_unsupported_media_type              -> 415, "Unsupported Media Type"
    | Status_requested_range_not_satisfiable -> 416, "Range Not Satisfiable"
    | Status_expectation_failed              -> 417, "Expectation Failed"

    (* Server Error 5xx *)
    | Status_internal_server_error           -> 500, "Internal Server Error"
    | Status_not_implemented                 -> 501, "Not Implemented"
    | Status_bad_gateway                     -> 502, "Bad Gateway"
    | Status_service_unavailable             -> 503, "Service Unavailable"
    | Status_gateway_timeout                 -> 504, "Gateway Timeout"
    | Status_version_not_supported           -> 505, "HTTP Version Not Supported"

    (* Other *)
    | Status_other (code, reason)            -> code, reason

  type cursor =
    | Start
    | In_version of char list
    | Version_SP
    | In_status_code of int * int
    | Status_code_SP
    | In_reason_phrase of char list
    | Resp_line_CR
    | In_headers of Headers.state
    | Done

  let string_of_cursor = function
    | Start ->
        "Start"
    | In_version cl ->
        Printf.sprintf "In-version \"%s\"" (rev_string_of_chars cl)
    | Version_SP ->
        "Version-SP"
    | In_status_code (sc, nd) ->
        Printf.sprintf "In-status-code %d (after %d digits)" sc nd
    | Status_code_SP ->
        "Status-code-SP"
    | In_reason_phrase cl ->
        Printf.sprintf "In-reason-phrase \"%s\"" (rev_string_of_chars cl)
    | Resp_line_CR ->
        "Resp-line-CR"
    | In_headers hs ->
        Printf.sprintf "In-headers (%s)" (Headers.string_of_cursor hs.Headers.cursor)
    | Done ->
        "Done"

  type state = {
    mutable cursor: cursor;
    mutable s_version: version option;
    mutable s_status_code: int option;
    mutable s_reason_phrase: string option;
    mutable s_headers: header_fields;
    mutable num_bytes_parsed: int
  }

  type error =
    | Unsupported_version of string
    | Parse_error of cursor * char
    | Unsupported_status_code of int
    | Invalid_reason_char of char
    | Internal_error of string

  exception Http_error of error

  let string_of_error = function
    | Unsupported_version s ->
        Printf.sprintf "Unsupported version in response: \"%s\"" s
    | Parse_error (cursor, c) ->
        Printf.sprintf "Response parse error in state %s at char '%C'"
          (string_of_cursor cursor) c
    | Unsupported_status_code sc ->
        Printf.sprintf "Unsupported status code %d" sc
    | Invalid_reason_char c ->
        Printf.sprintf "Invalid char in reason '%C'" c
    | Internal_error s ->
        Printf.sprintf "Internal response-parsing error: %s" s

  let raise_error err = raise (Http_error err)

  let init_state () = {
    cursor = Start;
    s_version = None;
    s_status_code = None;
    s_reason_phrase = None;
    s_headers = [];
    num_bytes_parsed = 0
  }

  let num_bytes_parsed state =
    if state.cursor = Start then 0 else state.num_bytes_parsed

  (* From http://tools.ietf.org/html/rfc1945#section-4:
     Simple-Response = [ Entity-Body ]                               ; HTTP/0.9
     Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF ; HTTP/{1.0,1.1}

     So, we can only be parsing a full status line.
  *)

  let raise_bad_char s c = raise_error (Parse_error (s.cursor, c))
  let parse_char s c =
    (* dbg "parsing %C in state %s..." c (string_of_cursor s.cursor); *)
      match s.cursor with
        | Start ->
            if is_token_char c then s.cursor <- In_version [ c ]
            else if not (is_space c) then raise_bad_char s c
        | In_version cl ->
            (match c with
               | ' ' | '\t' ->
                   (match rev_string_of_chars cl with
                      | "HTTP/1.0" -> s.s_version <- Some HTTP10
                      | "HTTP/1.1" -> s.s_version <- Some HTTP11
                      | v -> raise_error (Unsupported_version v)
                   );
                   s.cursor <- Version_SP
               | _ when List.length cl > 8 ->
                   raise_error (Unsupported_version (rev_string_of_chars cl))
               | _ ->
                   s.cursor <- In_version (c :: cl)
            )
        | Version_SP ->
            (match c with
               | ' ' | '\t' -> ()
               | _ when Httputils.is_digit c -> s.cursor <- In_status_code ((Httputils.digit_value c), 1)
               | _ -> raise_bad_char s c
            )
        | In_status_code (sc, nd) ->
            (match c with
               | ' ' | '\t' ->
                   s.s_status_code <- Some sc;
                   s.cursor <- Status_code_SP
               | '\r' ->
                   s.s_status_code <- Some sc;
                   s.cursor <- Resp_line_CR
               | '\n' ->
                   s.s_status_code <- Some sc;
                   s.cursor <- In_headers (Headers.init_state ())
               | _ when Httputils.is_digit c ->
                   let nsc = 10 * sc + (Httputils.digit_value c) in
                     if nd >= 3 then raise_error (Unsupported_status_code nsc)
                     else s.cursor <- In_status_code (nsc, nd + 1)
               | _ ->
                   raise_bad_char s c
            )
        | Status_code_SP ->
            (match c with
               | ' ' | '\t' -> ()
               | '\r' -> s.cursor <- Resp_line_CR
               | '\n' -> s.cursor <- In_headers (Headers.init_state ())
               | _ when is_ctl_char c ->
                   (* control chars not allowed in the reason phrase *)
                   raise_error (Invalid_reason_char c)
               | _ -> s.cursor <- In_reason_phrase [ c ])
        | In_reason_phrase clist ->
            (match c with
               | ' ' | '\t' -> ()
               | '\r' | '\n' ->
                   s.s_reason_phrase <- Some (rev_string_of_chars clist);
                   s.cursor <- (if c = '\r'
                                then Resp_line_CR
                                else In_headers (Headers.init_state ()))
               | _ when is_ctl_char c ->
                   (* control chars not allowed in the reason phrase *)
                   raise_error (Invalid_reason_char c)
               | _ -> s.cursor <- In_reason_phrase (c :: clist)
            )
        | Resp_line_CR ->
            if c = '\n' then s.cursor <- In_headers (Headers.init_state ())
            else raise_bad_char s c
        | In_headers hs ->
            Headers.parse_char hs c;
            if Headers.is_done hs then begin
              s.s_headers <- List.rev hs.Headers.headers;
              s.cursor <- Done
            end
        | Done -> raise_error (Internal_error "parse called on finished request!")

  type t = {
    version: version;
    status_code: int;
    reason_phrase: string;
    headers: header_fields;
  }

  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state

  let get_parse_result state =
    match state.cursor with
      | Done -> Some { version = optval state.s_version;
                       status_code = optval state.s_status_code;
                       reason_phrase = optval state.s_reason_phrase;
                       headers = state.s_headers
                     }
      | _    -> None

  let parse_substring state str ofs len =
    let i = ref ofs in
    let iend = ofs + len in
      while get_parse_result state = None && !i < iend do
        parse_char state str.[!i];
        incr i;
        state.num_bytes_parsed <- state.num_bytes_parsed + 1
      done;
      match get_parse_result state with
        | Some v -> Result (v, !i - ofs)
        | None -> Parse_incomplete state

  let parse state str =
    parse_substring state str 0 (String.length str)

  let serialize buf resp =
    if resp.version <> HTTP09 then begin
      Buffer.add_string buf (string_of_version resp.version);
      Buffer.add_string buf (Printf.sprintf " %d " resp.status_code);
      Buffer.add_string buf resp.reason_phrase;
      Buffer.add_string buf "\r\n";
      Headers.serialize buf resp.headers
    end
end


(* Note: The following are currently unsupported:
   . payloads using multipart/byteranges (http://tools.ietf.org/html/rfc2616#section-19.2)
*)

type payload_callback = string -> (* offset *) int -> (* length *) int -> (* final *) bool -> unit

module Payload = struct
  type cursor =
    | Start_chunk_length
    | In_chunk_length
    | Chunk_length_CR
    | In_chunk_extension
    | In_chunk
    | Chunk_CR
    | In_body
    | In_trailer of Headers.state
    | Done

  let string_of_cursor = function
    | Start_chunk_length -> "Start-chunk-length"
    | In_chunk_length    -> "In-chunk-length"
    | Chunk_length_CR    -> "Chunk-length-CR"
    | In_chunk_extension -> "In-chunk-extension"
    | In_chunk           -> "In-chunk"
    | Chunk_CR           -> "Chunk-CR"
    | In_body            -> "In-body"
    | In_trailer hs ->
        Printf.sprintf "In-trailer (%s)" (Headers.string_of_cursor hs.Headers.cursor)
    | Done               -> "Done"

  type content_length =
    | Chunked
    | Length of int64
    | Connection_close

  (* if a payload callback is registered, this governs the max
     amount of buffered data before the callback is called.
  *)
  let max_buffered_size = 2048

  type state = {
    mutable cursor: cursor;
    content_length: content_length;
    mutable remaining_length: int64;
    max_payload_length: int64;
    mutable body: Buffer.t;
    payload_callback: payload_callback option;
    mutable headers: header_fields;
    mutable num_bytes_parsed: int64
  }

  let num_bytes_parsed state = state.num_bytes_parsed

  type error =
    | Parse_error of cursor * char
    | Max_payload_length_exceeded of int64
    | Internal_error of string

  let string_of_error = function
    | Parse_error (cursor, c) ->
        Printf.sprintf "Payload parse error in state %s at char '%C'"
          (string_of_cursor cursor) c
    | Max_payload_length_exceeded mx ->
        Printf.sprintf "Exceeded max-payload-length %Ld" mx
    | Internal_error s ->
        Printf.sprintf "Internal payload-parsing error: %s" s

  exception Http_error of error

  let raise_error err = raise (Http_error err)

  let transfer_encoding_uses_chunked hdrs =
    (* http://tools.ietf.org/html/rfc2616#section-4.4, Item 2. *)
    try
      match lookup_header "Transfer-Encoding" hdrs with
        | vals when List.length vals >= 2 -> true
        | v :: _ ->
            let identity = "identity" in
            let vl, il = String.length v, String.length identity in
              vl < il || String.lowercase (String.sub v 0 il) <> identity
        | [] -> (* should never happen *) false
    with Not_found -> false

  let get_content_length version headers =
    try
      match lookup_header "Content-Length" headers with
        | v :: _ -> (try `Some (Int64.of_string v) with Failure _ -> `Error)
        | [] -> (* should never happen *) `None
    with Not_found -> if version = HTTP09 then `HTTP09 else `None

  let content_type_is_multipart_byteranges hdrs =
    try
      match lookup_header "Content-Type" hdrs with
        | v :: _ ->
            let multipart = "multipart/byteranges" in
            let vl, ml = String.length v, String.length multipart in
              vl >= ml && String.lowercase (String.sub v 0 ml) = multipart
        | [] -> (* should never happen *) false
    with Not_found -> false

  type payload_type =
    | No_payload
    | Payload of state
    | Error of string

  let init_default_state payload_callback max_payload_length = {
    cursor = In_body;
    content_length = Connection_close;
    remaining_length = -1L;
    max_payload_length = max_payload_length;
    body = Buffer.create 512;
    payload_callback = payload_callback;
    headers = [];
    num_bytes_parsed = 0L
  }

  let default_max_payload_length = Int64.of_int (10*1024*1024)

  let init_from_request ?payload_callback ?(max_payload_length=default_max_payload_length) req =
    let version = req.Request_header.version in
    let meth = req.Request_header.meth in
    let hdrs = req.Request_header.headers in
    let chunked = transfer_encoding_uses_chunked hdrs in
    let content_length = get_content_length version hdrs in
    let multipart_body = content_type_is_multipart_byteranges hdrs in
    let default = init_default_state payload_callback max_payload_length in

      match version, meth, content_length, chunked, multipart_body with
        | HTTP09, _, _, _, _
        | _, Request_header.Connect, _, _, _  -> No_payload

        (* We need to check Transfer-Encoding before Content-Length.
           http://tools.ietf.org/html/rfc2616#section-4.4, Item 3.
        *)
        | _, _, _, true, false          -> Payload { default with
                                                       cursor = Start_chunk_length;
                                                       content_length = Chunked
                                                   }
        | _, _, `HTTP09, _, false
        | HTTP11, _, `None, _, false    -> No_payload
        | _, _, `Some 0L, _, false      -> No_payload
        | _, _, `Some l, _, false       -> Payload { default with
                                                       content_length = Length l;
                                                       remaining_length = l
                                                   }
        | _, _, `Error, _, false        -> Error "Invalid Content-Length"
        | _, _, _, _, true              -> Error "multipart/byteranges is unsupported"

        (* Default to assuming that the payload is terminated by a Connection:close. *)
        | _                             -> Payload default

  let init_from_response ?payload_callback ?(max_payload_length=default_max_payload_length) resp =
    let version = resp.Response_header.version in
    let status_code = resp.Response_header.status_code in
    let hdrs = resp.Response_header.headers in
    let chunked = transfer_encoding_uses_chunked hdrs in
    let content_length = get_content_length version hdrs in
    let multipart_body = content_type_is_multipart_byteranges hdrs in
    let default = init_default_state payload_callback max_payload_length in

      match status_code, content_length, chunked, multipart_body with
        | sc, _, _, _
            (* http://tools.ietf.org/html/rfc2616#section-4.4, Item 1. *)
            when sc/100 = 1 || sc = 204 || sc = 304 -> No_payload

        (* We need to check Transfer-Encoding before Content-Length.
           http://tools.ietf.org/html/rfc2616#section-4.4, Item 3.
        *)
        | _, _, true, _     -> Payload { default with
                                           cursor = Start_chunk_length;
                                           content_length = Chunked
                                       }

        | _, `HTTP09, _, _  -> Payload default (* Connection: close *)
        | _, `Some 0L, _, _ -> No_payload
        | _, `Some l, _, _  -> Payload { default with
                                           content_length = Length l;
                                           remaining_length = l
                                       }
        | _, `Error, _, _   -> Error "Invalid Content-Length"

        | _, _, _, true     -> Error "multipart/byteranges is currently unsupported"

        (* Default to assuming that the payload is terminated by a Connection:close. *)
        | _ ->                 Payload default

  let check_payload_callback s final =
    match s.payload_callback with
      | None -> ()
      | Some f when final || Buffer.length s.body >= max_buffered_size ->
          let content = Buffer.contents s.body in
            Buffer.clear s.body;
            f content 0 (String.length content) final
      | _ -> ()

  let raise_bad_char s c = raise_error (Parse_error (s.cursor, c))
  let parse_char s c =
    (* dbg "parsing %C in state %s..." c (string_of_cursor s.cursor); *)
      match s.cursor with
        | In_body ->
            check_payload_callback s false;
            Buffer.add_char s.body c;
            if s.remaining_length > 0L then begin
              s.remaining_length <- Int64.pred s.remaining_length;
              if s.remaining_length = 0L then begin
                s.cursor <- Done;
                check_payload_callback s true
              end
            end
        | Start_chunk_length ->
            if Httputils.is_hex c then begin
              s.remaining_length <- Int64.of_int (Httputils.hex_value c);
              s.cursor <- In_chunk_length
            end else raise_bad_char s c
        | In_chunk_length ->
            (match c with
               | '\r' ->
                   s.cursor <- Chunk_length_CR
               | '\n' ->
                   if s.remaining_length = 0L
                   then s.cursor <- In_trailer (Headers.init_state ())
                   else s.cursor <- In_chunk
               | ' ' | '\t' | ';' ->
                   s.cursor <- In_chunk_extension
               | _ when Httputils.is_hex c ->
                   (* TODO: check for overflow!! *)
                   s.remaining_length <- (Int64.add (Int64.shift_left s.remaining_length 4)
                                            (Int64.of_int (Httputils.hex_value c)))
               | _ -> raise_bad_char s c
            )
        | In_chunk_extension ->
            (match c with
               | '\r' ->
                   s.cursor <- Chunk_length_CR
               | '\n' ->
                   if s.remaining_length = 0L
                   then s.cursor <- In_trailer (Headers.init_state ())
                   else s.cursor <- In_chunk
               | _ -> ()
            )
        | Chunk_length_CR ->
            if c = '\n' then begin
              if s.remaining_length = 0L
              then s.cursor <- In_trailer (Headers.init_state ())
              else s.cursor <- In_chunk
            end else raise_bad_char s c
        | In_chunk ->
            if s.remaining_length = 0L then
              (match c with
                 | '\r' -> s.cursor <- Chunk_CR
                 | '\n' -> s.cursor <- Start_chunk_length
                 | _    -> raise_bad_char s c
              )
            else begin
              check_payload_callback s false;
              Buffer.add_char s.body c;
              s.remaining_length <- Int64.pred s.remaining_length
            end
        | Chunk_CR ->
            if c = '\n' then s.cursor <- Start_chunk_length
            else raise_bad_char s c
        | In_trailer hs ->
            Headers.parse_char hs c;
            if Headers.is_done hs then begin
              s.headers <- List.rev hs.Headers.headers;
              s.cursor <- Done;
              check_payload_callback s true
            end
        | Done -> raise_error (Internal_error "parse called on finished request!")

  type t = {
    content: Buffer.t;
    trailers: header_fields
  }

  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state

  let get_parse_result state =
    match state.cursor with
      | Done -> Some { content = state.body;
                       trailers = state.headers
                     }
      | _    -> None

  let can_optimize state =
    match state.cursor with
      | In_body  when state.remaining_length > 1L   -> true
      | In_chunk when state.remaining_length > 1L   -> true
      | _                                           -> false

  let parse_optimized state str ofs len =
    let len64 = Int64.of_int len in
    let chomp64 = (if state.remaining_length <= len64
                   then Int64.sub state.remaining_length 1L
                   else len64) in
    let chomp = Int64.to_int chomp64 in
      (* dbg " chomped %d bytes in state %s..." chomp (string_of_cursor state.cursor); *)
      Buffer.add_substring state.body str ofs chomp;
      state.remaining_length <- Int64.sub state.remaining_length chomp64;
      chomp

  let parse_unoptimized state str ofs len =
    let i = ref ofs in
    let iend = ofs + len in
      while get_parse_result state = None
          && !i < iend
          && not (can_optimize state)
      do
        parse_char state str.[!i];
        incr i;
        state.num_bytes_parsed <- Int64.succ state.num_bytes_parsed
      done;
      match get_parse_result state with
        | Some v -> `Done (v, !i - ofs)
        | None -> (if !i = iend
                   then `Incomplete
                   else `Continue_optimized (!i - ofs))

  let parse_substring state str ofs len =
    let rec helper o l acc =
      if can_optimize state then begin
        let chomp = parse_optimized state str o l in
          helper (o + chomp) (l - chomp) (acc + chomp)
      end else begin
        match parse_unoptimized state str o l with
          | `Done (v, consumed) ->
              Result (v, acc + consumed)
          | `Incomplete ->
              Parse_incomplete state
          | `Continue_optimized consumed ->
              helper (o + consumed) (l - consumed) (acc + consumed)
      end
    in helper ofs len 0

  let parse state str =
    parse_substring state str 0 (String.length str)

  let connection_closed state =
    if state.content_length = Connection_close then begin
      state.cursor <- Done;
      check_payload_callback state true
    end

  let serialize ~chunked buf payload =
    if chunked then begin
      let len = Buffer.length payload.content in
        Buffer.add_string buf (Printf.sprintf "%dx\r\n" len);
        Buffer.add_buffer buf payload.content;
        Buffer.add_string buf "0\r\n";
        Headers.serialize buf payload.trailers
    end else begin
      Buffer.add_buffer buf payload.content
    end

  let serialize_of_request req buf payload =
    let chunked = transfer_encoding_uses_chunked req.Request_header.headers in
      serialize ~chunked buf payload

  let serialize_of_response resp buf payload =
    let chunked = transfer_encoding_uses_chunked resp.Response_header.headers in
      serialize ~chunked buf payload
end


module Request = struct
  type cursor =
    | In_request_header of Request_header.state
    | In_payload of Payload.state
    | Done

  type header_callback = Request_header.t -> Request_header.t

  type state = {
    mutable cursor: cursor;
    mutable s_request: Request_header.t option;
    mutable num_bytes_parsed: int64;
    header_callback: header_callback option;
    payload_callback: payload_callback option;
  }

  type error =
    | Internal_error of string

  let string_of_error = function
    | Internal_error s ->
        Printf.sprintf "Internal request-parsing error: %s" s

  exception Http_error of error

  let raise_error err = raise (Http_error err)

  let init_state ?header_callback ?payload_callback () = {
    cursor = In_request_header (Request_header.init_state ());
    s_request = None;
    num_bytes_parsed = 0L;
    header_callback = header_callback;
    payload_callback = payload_callback
  }

  let num_bytes_parsed s =
    match s.cursor with
      | In_request_header rs -> Int64.of_int (Request_header.num_bytes_parsed rs)
      | In_payload ps        -> Int64.add s.num_bytes_parsed (Payload.num_bytes_parsed ps)
      | Done                 -> s.num_bytes_parsed

  type t = {
    request: Request_header.t;
    payload: Payload.t option;
  }

  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
    | Error of string

  let rec parse_helper state str ofs len pre_consumed =
    match state.cursor with
      | In_request_header rs ->
          (match Request_header.parse_substring rs str ofs len with
             | Request_header.Result (v, consumed) ->
                 let v = (match state.header_callback with
                            | None -> v
                            | Some f -> f v
                         ) in
                   state.s_request <- Some v;
                   state.num_bytes_parsed <- Int64.of_int (Request_header.num_bytes_parsed rs);
                   (match Payload.init_from_request ?payload_callback:state.payload_callback v with
                      | Payload.No_payload ->
                          state.cursor <- Done;
                          Result ({ request = v; payload = None }, pre_consumed + consumed)
                      | Payload.Error s ->
                          state.cursor <- Done;
                          Error s
                      | Payload.Payload ps ->
                          state.cursor <- In_payload ps;
                          (* recurse on remaining input *)
                          parse_helper state str (ofs + consumed) (len - consumed) (pre_consumed + consumed)
                   )
             | Request_header.Parse_incomplete rs ->
                 state.cursor <- In_request_header rs;
                 Parse_incomplete state
          )
      | In_payload ps ->
          (match Payload.parse_substring ps str ofs len with
             | Payload.Result (p, consumed) ->
                 state.num_bytes_parsed <- Int64.add state.num_bytes_parsed (Payload.num_bytes_parsed ps);
                 state.cursor <- Done;
                 Result ({ request = optval state.s_request; payload = Some p }, pre_consumed + consumed)
             | Payload.Parse_incomplete ps ->
                 state.cursor <- In_payload ps;
                 Parse_incomplete state
          )
      | Done -> raise_error (Internal_error "parse called on finished request!")

  let rec parse_substring state str ofs len =
    parse_helper state str ofs len 0

  let parse state str =
    parse_substring state str 0 (String.length str)

  let get_parse_result state =
    match state.cursor with
      | In_payload ps ->
          (match Payload.get_parse_result ps with
             | None -> None
             | Some p -> Some { request = optval state.s_request; payload = Some p })
      | _ -> None

  let connection_closed state =
    match state.cursor with
      | In_request_header _ -> ()
      | In_payload ps       -> Payload.connection_closed ps
      | Done                -> ()

  let serialize buf req =
    Request_header.serialize buf req.request;
    match req.payload with
      | Some p -> Payload.serialize_of_request req.request buf p
      | None   -> ()
end


module Response = struct
  type cursor =
    | In_response_header of Response_header.state
    | In_payload of Payload.state
    | Done

  type header_callback = Response_header.t -> Response_header.t

  type state = {
    mutable cursor: cursor;
    mutable s_response: Response_header.t option;
    mutable num_bytes_parsed: int64;
    header_callback: header_callback option;
    payload_callback: payload_callback option
  }

  type error =
    | Internal_error of string

  let string_of_error = function
    | Internal_error s ->
        Printf.sprintf "Internal response-parsing error: %s" s

  exception Http_error of error

  let raise_error err = raise (Http_error err)

  let init_state ?header_callback ?payload_callback () = {
    cursor = In_response_header (Response_header.init_state ());
    s_response = None;
    num_bytes_parsed = 0L;
    header_callback = header_callback;
    payload_callback = payload_callback
  }

  let num_bytes_parsed s =
    match s.cursor with
      | In_response_header rs -> Int64.of_int (Response_header.num_bytes_parsed rs)
      | In_payload ps         -> Int64.add s.num_bytes_parsed (Payload.num_bytes_parsed ps)
      | Done                  -> s.num_bytes_parsed

  type t = {
    response: Response_header.t;
    payload: Payload.t option;
  }

  type parse_result =
    | Result of t * (* number of consumed bytes *) int
    | Parse_incomplete of state
    | Error of string

  let rec parse_helper state str ofs len pre_consumed =
    match state.cursor with
      | In_response_header rs ->
          (match Response_header.parse_substring rs str ofs len with
             | Response_header.Result (v, consumed) ->
                 let v = (match state.header_callback with
                            | None -> v
                            | Some f -> f v
                         ) in
                   state.s_response <- Some v;
                   state.num_bytes_parsed <- Int64.of_int (Response_header.num_bytes_parsed rs);
                   (match Payload.init_from_response ?payload_callback:state.payload_callback v with
                      | Payload.No_payload ->
                          state.cursor <- Done;
                          Result ({ response = v; payload = None }, pre_consumed + consumed)
                      | Payload.Error s ->
                          state.cursor <- Done;
                          Error s
                      | Payload.Payload ps ->
                          state.cursor <- In_payload ps;
                          (* recurse on remaining input *)
                          parse_helper state str (ofs + consumed) (len - consumed) (pre_consumed + consumed)
                   )
             | Response_header.Parse_incomplete rs ->
                 state.cursor <- In_response_header rs;
                 Parse_incomplete state
          )
      | In_payload ps ->
          (match Payload.parse_substring ps str ofs len with
             | Payload.Result (p, consumed) ->
                 state.num_bytes_parsed <- Int64.add state.num_bytes_parsed (Payload.num_bytes_parsed ps);
                 state.cursor <- Done;
                 Result ({ response = optval state.s_response; payload = Some p }, pre_consumed + consumed)
             | Payload.Parse_incomplete ps ->
                 state.cursor <- In_payload ps;
                 Parse_incomplete state
          )
      | Done -> raise_error (Internal_error "parse called on finished response!")

  let rec parse_substring state str ofs len =
    parse_helper state str ofs len 0

  let parse state str =
    parse_substring state str 0 (String.length str)

  let get_parse_result state =
    match state.cursor with
      | In_payload ps ->
          (match Payload.get_parse_result ps with
             | None -> None
             | Some p -> Some { response = optval state.s_response; payload = Some p })
      | _ -> None

  let connection_closed state =
    match state.cursor with
      | In_response_header _ -> ()
      | In_payload ps        -> Payload.connection_closed ps
      | Done                 -> ()

  let serialize buf resp =
    Response_header.serialize buf resp.response;
    match resp.payload with
      | Some p -> Payload.serialize_of_response resp.response buf p
      | None   -> ()

  let make_response ?payload ~headers status =
    let code, reason = Response_header.status_info status in
    let payload_length = (match payload with
                            | None -> 0
                            | Some p -> Buffer.length p.Payload.content) in
    let headers = (if is_header_present "Content-Length" headers then headers
                   else (add_header "Content-Length" (Printf.sprintf "%d" payload_length)
                           headers)) in
    let resp_header = { Response_header.version = HTTP11;
                        Response_header.status_code = code;
                        Response_header.reason_phrase = reason;
                        Response_header.headers = headers }
    in { response = resp_header;
         payload = payload }
end
