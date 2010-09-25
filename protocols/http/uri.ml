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

(* This implements URI parsing and handling as specified in RFC 3986. *)

type authority = {
  userinfo : string option;
  host : string;
  port : int option;
}

type scheme = string

type t = {
  scheme : scheme option;
  authority : authority option;
  path : string option;
  query : string option;
  fragment : string option;
}

type error =
  | Invalid_uri of string
  | Invalid_authority of string
  | Invalid_port of string
  | Missing_required_components of string

let string_of_error = function
  | Invalid_uri s ->
      Printf.sprintf "\"%s\" is an invalid URI" s
  | Invalid_authority a ->
      Printf.sprintf "\"%s\" is an invalid authority" a
  | Invalid_port p ->
      Printf.sprintf "\"%s\" is an invalid port" p
  | Missing_required_components s ->
      Printf.sprintf "\"%s\" is an incomplete URI" s

exception Uri_error of error

let raise_error e =
  raise (Uri_error e)

let explode s =
  let rec unfold indx acc =
    if indx < 0 then acc
    else unfold (indx - 1) (s.[indx] :: acc)
  in unfold ((String.length s) - 1) []

let implode cl =
  let len = List.length cl in
  let s = String.create len in
    ignore (List.fold_left (fun idx c -> s.[idx] <- c; idx + 1) 0 cl);
    s

let lowercase s =
  implode (List.map Char.lowercase (explode s))

let maybe f x = match x with None -> None | Some x -> Some (f x)

let is_sub_delim = function
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false
let is_gen_delim = function
  | ':' | '/' | '?' | '#' | '[' | ']' | '@' -> true
  | _ -> false
let is_reserved c = is_gen_delim c || is_sub_delim c
let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | '-' | '.' | '_' | '~' -> true
  | _ -> false
let is_pchar c =
  is_unreserved c || is_sub_delim c || c = ':' || c = '@'

let pct_decode s =
  let cl = explode s in
  let rec decode cl acc =
    match cl with
      | '%':: h :: l :: rest when Httputils.is_hex h && Httputils.is_hex l ->
          let hn, ln = Httputils.hex_value h, Httputils.hex_value l in
          let chr = Char.chr ((hn lsl 4) + ln) in
            (* TODO: the is_unreserved check is the most
               conservative, but may be overly so.  We
               need to pass in the context in which the
               decoding is being done, represented as the
               character classes of allowed decoded
               characters.
            *)
            if is_unreserved chr then decode rest (chr :: acc)
            else decode rest (Char.uppercase l :: Char.uppercase h :: '%' :: acc)
      | c :: rest ->
          decode rest (c :: acc)
      | [] ->
          implode (List.rev acc)
  in decode cl []

(* This is based on the "remove_dot_segments" algorithm specified in
   Section 5.2.4 of RFC 3986.  Note that this algorithm passes empty
   path segments (e.g. the one between a and b in "a//b") through to
   the output.
*)
let remove_dot_segments path =
  let slash = Str.regexp "/" in
  let path_len = String.length path in
  let has_leading_slash = path_len > 0 && path.[0] = '/' in
  let has_trailing_slash = path_len > 0 && path.[path_len - 1] = '/' in
  let rec remove in_segs out_segs =
    match in_segs, out_segs with
      | "." :: in_rest, _ ->
          remove in_rest out_segs
      | ".." :: in_rest, _ :: out_rest ->
          remove in_rest out_rest
      | ".." :: in_rest, [] ->
          remove in_rest []
      | s :: in_rest, out_rest ->
          remove in_rest (s :: out_rest)
      | [], _ ->
          List.rev out_segs in
  let out_segs = remove (Str.split slash path) [] in
  let out = String.concat "/" out_segs in
  let out = if has_leading_slash then "/" ^ out else out in
    if has_trailing_slash && path <> "/" then out ^ "/" else out

(* Regular expression for authority = [userinfo "@"] host [":" port] *)
let auth_re = "\\(\\([^@]*\\)@\\)?\\([^:]*\\)\\(:\\([0-9]*\\)\\)?"

let parse_authority s =
  let get_opt_group n =
    try Some (Str.matched_group n s)
    with Not_found -> None in
  let get_opt_port () =
    match get_opt_group 5 with
      | None -> None
      | Some s ->
          if String.length s = 0 then None
          else (try Some (int_of_string s)
                with Failure _ -> raise_error (Invalid_port s)
               )
  in
  let auth_regexp = Str.regexp auth_re in
    if Str.string_match auth_regexp s 0 then
      let ret = Some { userinfo = get_opt_group 2;
                       host     = Str.matched_group 3 s;
                       port     = get_opt_port ();
                     }
      in
        if ret = Some { userinfo = None; host = ""; port = None }
        then None else ret
    else None

(* The following regular expression and explanation is taken from
   RFC3986, Appendix B:

   The following line is the regular expression for breaking-down a
   well-formed URI reference into its components.

   ^(([^:/?#]+):)?(//([^/?#]* ))?([^?#]* )(\?([^#]* ))?(#(.* ))?
   12        2 1 3  4        43 5       56  7      76 8 9   98

   For example, matching the above expression to

   http://www.ics.uci.edu/pub/ietf/uri/#Related

   results in the following subexpression matches:

   $1 = http:
   $2 = http
   $3 = //www.ics.uci.edu
   $4 = www.ics.uci.edu
   $5 = /pub/ietf/uri/
   $6 = <undefined>
   $7 = <undefined>
   $8 = #Related
   $9 = Related

   where <undefined> indicates that the component is not present, as
   is the case for the query component in the above example.
   Therefore, we can determine the value of the five components as

   scheme    = $2
   authority = $4
   path      = $5
   query     = $7
   fragment  = $9
*)

let scheme_re    = "\\([^:/\\?#]+\\)"  (* 2-2 *)
let authority_re = "\\([^/\\?#]*\\)"   (* 4-4 *)
let path_re      = "\\([^\\?#]*\\)"    (* 5-5 *)
let query_re     = "\\([^#]*\\)"       (* 7-7 *)
let frag_re      = "\\(.*\\)"          (* 9-9 *)

let uri_re       = (Printf.sprintf "^\\(%s:\\)?\\(//%s\\)?%s\\(\\?%s\\)?\\(#%s\\)?$"
                      scheme_re authority_re path_re query_re frag_re)

(* Get the five components of the uri using the regular expression
   above.  Note that an empty string matches the regular expression, is
   not a valid URI.

   Also, the Str library uses static state, so we need to be careful
   with interleaving calls to it with two or more regular expressions
   at the same time (e.g. authority and uri) below.
*)
let parse_uri s =
  let uri_regexp = Str.regexp uri_re in
  let get_authority a =
    match parse_authority a with
      | None when String.length a > 0
          -> raise_error (Invalid_authority a)
      | auth -> auth
  in
    if Str.string_match uri_regexp s 0 then
      let get_opt_group n =
        try Some (Str.matched_group n s)
        with Not_found -> None in
      let scheme    = get_opt_group 2 in
      let auth      = get_opt_group 4 in
      let path      = get_opt_group 5 in
      let query     = get_opt_group 7 in
      let fragment  = get_opt_group 9 in
        (* Parse authority regexp _after_ finishing with the uri regexp. *)
      let authority = (match auth with
                         | None -> None
                         | Some a -> get_authority a
                      ) in
        if scheme = None && path = None then
          raise_error (Missing_required_components s);
        {
          scheme    = scheme;
          authority = authority;
          path      = path;
          query     = query;
          fragment  = fragment;
        }
    else raise_error (Invalid_uri s)

let of_string s = parse_uri s

(* See Section 5.3 of RFC 3986. *)
let authority_to_string a =
  let mod_host = match a.userinfo with None -> a.host | Some u -> u ^ "@" ^ a.host
  in match a.port with None -> mod_host | Some p -> mod_host ^ ":" ^ string_of_int p

let to_string u =
  let scheme =
    match u.scheme with None -> "" | Some s -> s ^ ":" in
  let append_authority acc =
    match u.authority with None -> acc | Some a -> acc ^ "//" ^ (authority_to_string a) in
  let append_path acc =
    match u.path with None -> acc | Some p -> acc ^ p in
  let append_query acc =
    match u.query with None -> acc | Some q -> acc ^ "?" ^ q in
  let append_fragment acc =
    match u.fragment with None -> acc | Some f -> acc ^ "#" ^ f
  in append_fragment (append_query (append_path (append_authority scheme)))

(* See Section 6 of RFC 3986.  This function implements case
   normalization, percent-encoding normalization, and path segment
   normalization, as described in Sections 6.2.2.1 through 6.2.2.3.
*)
let normalize_auth a =
  { a with
      userinfo = maybe pct_decode a.userinfo;
      host = lowercase a.host;
  }

let normalize u =
  let scheme = maybe lowercase u.scheme in
  let authority = maybe normalize_auth u.authority in
    (* Note that pct_decode needs to be done _before_
       remove_dot_segments, so that we can process any encoded "."
       characters (which are in the unreserved set).  Otherwise,
       the embedded "." characters will escape their processing by
       remove_dot_segments.
    *)
  let path = maybe (fun s -> remove_dot_segments (pct_decode s)) u.path in
  let query = maybe pct_decode u.query in
  let fragment = maybe pct_decode u.fragment in
    { scheme = scheme;
      authority = authority;
      path = path;
      query = query;
      fragment = fragment;
    }
