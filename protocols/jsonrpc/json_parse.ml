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

type sign =
  | Pos
  | Neg

type cursor =
  | Start
  | Expect_value
  | In_null of int
  | In_true of int
  | In_false of int
  | In_int_sign of sign
  | In_int_zero of sign
  | In_int of sign * int
  | In_float of sign * int * char list
  | In_int_expsign of sign * int
  | In_int_expdig of sign * int * sign
  | In_int_exp of sign * int * sign * int
  | In_float_expsign of sign * int * char list
  | In_float_expdig of sign * int * char list * sign
  | In_float_exp of sign * int * char list * sign * int
  | In_string of Buffer.t
  | In_string_control of Buffer.t
  | In_string_hex of Buffer.t * char list * int
  | Expect_object_elem_start
  | Expect_object_elem_colon
  | Expect_comma_or_end
  | Expect_object_key
  | Done of Json.t

type int_value =
  | IObject of (string * Json.t) list
  | IObject_needs_key of (string * Json.t) list
  | IObject_needs_value of (string * Json.t) list * string
  | IArray of Json.t list

type parse_state = {
  mutable cursor: cursor;
  mutable stack: int_value list;
  mutable num_chars_parsed: int;
  mutable line_num: int;
  mutable string_buffer_size: int;
}

let init_parse_state () = {
  cursor = Start;
  stack = [];
  num_chars_parsed = 0;
  line_num = 1;
  string_buffer_size = 512;
}

let is_parsing_object s =
  match s.stack with
    | IObject _ :: _ | IObject_needs_key _ :: _ | IObject_needs_value _ :: _ -> true
    | IArray _ :: _
    | [] -> false

let get_parse_result s =
  match s.cursor with
    | Done v -> Some v
    | _ -> None

let ivalue_to_str = function
  | IObject _ -> "object"
  | IObject_needs_key _ -> "object_needing_key"
  | IObject_needs_value _ -> "object_needing_value"
  | IArray _ -> "array"

let current_cursor_value = function
  | Start | Expect_value -> "value"
  | In_null _ -> "null"
  | In_true _ | In_false _ -> "boolean"
  | In_int_sign _ | In_int_zero _ | In_int _ | In_int_expsign _ | In_int_expdig _ | In_int_exp _
  | In_float _ | In_float_expsign _ | In_float_expdig _ | In_float_exp _  -> "number"
  | In_string _ | In_string_control _ | In_string_hex _ -> "string"
  | Expect_object_elem_start | Expect_object_elem_colon | Expect_object_key -> "object"
  | Expect_comma_or_end -> "object/array"
  | Done _ -> ""

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let dig c = Char.code c - (* Char.code '0' *) 48

let update_line_num s c =
  if c = '\n' then
    s.line_num <- s.line_num + 1

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_valid_unescaped_char c =
  match (Char.code c) with
    | 0x22 | 0x5c -> false
    | x when 0x20 <= x && x <= 0x7f -> true  (* only ASCII for now *)
    | _ -> false

let clist_to_string cs =
  let len = List.length cs in
  let s = String.create len in
  let rec iter indx = function
    | c :: cs ->
        String.set s indx c;
        iter (indx + 1) cs
    | [] -> () in
    iter 0 cs;
    s

type error =
  | Unexpected_char of int * char * (* json type *) string
  | Invalid_value of int * (* value *) string * (* json type *) string
  | Invalid_leading_zero of int * string
  | Unterminated_value of int * string
  | Internal_error of int * string

let string_of_error = function
  | Unexpected_char (l, c, state) ->
      Printf.sprintf "Line %d: Unexpected char %C (x%X) encountered in state %s"
        l c (Char.code c) state
  | Invalid_value (l, v, t) ->
      Printf.sprintf "Line %d: '%s' is an invalid %s" l v t
  | Invalid_leading_zero (l, s) ->
      Printf.sprintf "Line %d: '%s' should not have leading zeros" l s
  | Unterminated_value (l, s) ->
      Printf.sprintf "Line %d: unterminated %s" l s
  | Internal_error (l, m) ->
      Printf.sprintf "Line %d: Internal error: %s" l m

exception Parse_error of error

let raise_unexpected_char s c t =
  raise (Parse_error (Unexpected_char (s.line_num, c, t)))
let raise_invalid_value s v t =
  raise (Parse_error (Invalid_value (s.line_num, v, t)))
let raise_invalid_leading_zero s n =
  raise (Parse_error (Invalid_leading_zero (s.line_num, n)))
let raise_unterminated_value s v =
  raise (Parse_error (Unterminated_value (s.line_num, v)))
let raise_internal_error s m =
  raise (Parse_error (Internal_error (s.line_num, m)))

let finish_value s v =
  match s.stack, v with
    | [], _ ->
        s.cursor <- Done v
    | IObject_needs_key fields :: tl, Json.String key ->
        s.stack <- IObject_needs_value (fields, key) :: tl;
        s.cursor <- Expect_object_elem_colon
    | IObject_needs_value (fields, key) :: tl, _ ->
        s.stack <- IObject ((key, v) :: fields) :: tl;
        s.cursor <- Expect_comma_or_end
    | IArray l :: tl, _ ->
        s.stack <- IArray (v :: l) :: tl;
        s.cursor <- Expect_comma_or_end
    | io :: _tl, _ ->
        raise_internal_error s ("unexpected " ^ (ivalue_to_str io)
                                ^ " on stack at finish_value")

let pop_stack s =
  match s.stack with
    | IObject fields :: tl ->
        s.stack <- tl;
        finish_value s (Json.Object (Array.of_list (List.rev fields)))
    | IArray l :: tl ->
        s.stack <- tl;
        finish_value s (Json.Array (Array.of_list (List.rev l)))
    | io :: _tl ->
        raise_internal_error s ("unexpected " ^ (ivalue_to_str io)
                                ^ " on stack at pop_stack")
    | [] ->
        raise_internal_error s "empty stack at pop_stack"

let int_of sign d =
  let d64 = Int64.of_int d in
    if sign = Neg then Int64.neg d64 else d64

let finish_int s ds d =
  finish_value s (Json.Int (int_of ds d))

let finish_int_exp s ds d es e =
  let int = int_of ds d in
  let exp = int_of es e in
  let str = Printf.sprintf "%Ld.e%Ld" int exp in
    (* If exp is positive, we might actually
       succeed in making this an int, but
       returning float is more uniform. *)
  let float = try float_of_string str
  with Failure _ -> raise_invalid_value s str "float" in
    finish_value s (Json.Float float)

let finish_float s ds d fs =
  let int = int_of ds d in
  let frac = clist_to_string (List.rev fs) in
  let str = Printf.sprintf "%Ld.%s" int frac in
  let float = try float_of_string str
  with Failure _ -> raise_invalid_value s str "float" in
    finish_value s (Json.Float float)

let finish_float_exp s ds d fs es e =
  let int = int_of ds d in
  let frac = clist_to_string (List.rev fs) in
  let exp = int_of es e in
  let str = Printf.sprintf "%Ld.%se%Ld" int frac exp in
  let float = try float_of_string str
  with Failure _ -> raise_invalid_value s str "float" in
    finish_value s (Json.Float float)

let rec parse_char s c =
  (* Printf.printf "parsing %C at line %d in state %s...\n" c s.line_num (current_cursor_value s.cursor); *)
    match s.cursor with
      | Start ->
          (match c with
             | 'n' ->
                 s.cursor <- In_null 3
             | 't' ->
                 s.cursor <- In_true 3
             | 'f' ->
                 s.cursor <- In_false 4
             | '-' ->
                 s.cursor <- In_int_sign Neg
             | '0' ->
                 s.cursor <- In_int_zero Pos
             | '1' .. '9' ->
                 s.cursor <- In_int (Pos, dig c)
             | '"' ->
                 s.cursor <- In_string (Buffer.create s.string_buffer_size)
             | '{' ->
                 s.cursor <- Expect_object_elem_start
             | '[' ->
                 s.stack  <- (IArray []) :: s.stack;
             | ']' when s.stack <> [] ->
                 pop_stack s
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "start")
      | Expect_value ->
          (match c with
             | 'n' ->
                 s.cursor <- In_null 3
             | 't' ->
                 s.cursor <- In_true 3
             | 'f' ->
                 s.cursor <- In_false 4
             | '-' ->
                 s.cursor <- In_int_sign Neg
             | '0' ->
                 s.cursor <- In_int_zero Pos
             | '1' .. '9' ->
                 s.cursor <- In_int (Pos, dig c)
             | '"' ->
                 s.cursor <- In_string (Buffer.create s.string_buffer_size)
             | '{' ->
                 s.cursor <- Expect_object_elem_start
             | '[' ->
                 s.stack  <- (IArray []) :: s.stack;
                 s.cursor <- Start
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "value")
      | In_null rem ->
          (match c, rem with
             | 'u', 3 ->
                 s.cursor <- In_null 2
             | 'l', 2 ->
                 s.cursor <- In_null 1
             | 'l', 1 ->
                 finish_value s Json.Null
             | _ ->
                 raise_unexpected_char s c "null")
      | In_true rem ->
          (match c, rem with
             | 'r', 3 ->
                 s.cursor <- In_true 2
             | 'u', 2 ->
                 s.cursor <- In_true 1
             | 'e', 1 ->
                 finish_value s (Json.Bool true)
             | _ ->
                 raise_unexpected_char s c "true")
      | In_false rem ->
          (match c, rem with
             | 'a', 4 ->
                 s.cursor <- In_false 3
             | 'l', 3 ->
                 s.cursor <- In_false 2
             | 's', 2 ->
                 s.cursor <- In_false 1
             | 'e', 1 ->
                 finish_value s (Json.Bool false)
             | _ ->
                 raise_unexpected_char s c "false")
      | In_int_sign ds ->
          (match c with
             | '0' ->
                 s.cursor <- In_int_zero ds
             | '1' .. '9' ->
                 s.cursor <- In_int (ds, dig c)
             | _ ->
                 raise_unexpected_char s c "int_sign")
      | In_int_zero ds ->
          (match c with
             | '.' ->
                 s.cursor <- In_float (ds, 0, [])
             | 'e' | 'E' ->
                 s.cursor <- In_int_expsign (ds, 0)
             | ',' | ']' | '}' ->
                 finish_int s ds 0;
                 parse_char s c
             | _ when is_space c ->
                 update_line_num s c;
                 finish_int s ds 0
             | _ ->
                 raise_unexpected_char s c "int_zero")
      | In_int (ds, d) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_int (ds, 10 * d + dig c)
             | '.' ->
                 s.cursor <- In_float (ds, d, [])
             | 'e' | 'E' ->
                 s.cursor <- In_int_expsign (ds, d)
             | ',' | ']' | '}' ->
                 finish_int s ds d;
                 parse_char s c
             | _ when is_space c ->
                 update_line_num s c;
                 finish_int s ds d
             | _ ->
                 raise_unexpected_char s c "int")
      | In_float (ds, d, fs) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_float (ds, d, c :: fs)
             | 'e' | 'E' ->
                 s.cursor <- In_float_expsign (ds, d, fs)
             | ',' | ']' | '}' ->
                 finish_float s ds d fs;
                 parse_char s c
             | _ when is_space c ->
                 update_line_num s c;
                 finish_float s ds d fs
             | _ ->
                 raise_unexpected_char s c "float")
      | In_int_expsign (ds, d) ->
          (match c with
             | '+' ->
                 s.cursor <- In_int_expdig (ds, d, Pos)
             | '0' .. '9' ->
                 s.cursor <- In_int_exp (ds, d, Pos, dig c)
             | '-' ->
                 s.cursor <- In_int_expdig (ds, d, Neg)
             | _ ->
                 raise_unexpected_char s c "int_expsign"
          )
      | In_int_expdig (ds, d, es) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_int_exp (ds, d, es, dig c)
             | _ ->
                 raise_unexpected_char s c "int_expdig")
      | In_int_exp (ds, d, es, e) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_int_exp (ds, d, es, 10 * e + dig c)
             | ',' | ']' | '}' ->
                 finish_int_exp s ds d es e;
                 parse_char s c
             | _ when is_space c ->
                 update_line_num s c;
                 finish_int_exp s ds d es e
             | _ ->
                 raise_unexpected_char s c "int_exp")
      | In_float_expsign (ds, d, fs) ->
          (match c with
             | '+' ->
                 s.cursor <- In_float_expdig (ds, d, fs, Pos)
             | '0' .. '9' ->
                 s.cursor <- In_float_exp (ds, d, fs, Pos, dig c)
             | '-' ->
                 s.cursor <- In_float_expdig (ds, d, fs, Neg)
             | _ ->
                 raise_unexpected_char s c "float_expsign")
      | In_float_expdig (ds, d, fs, es) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_float_exp (ds, d, fs, es, dig c)
             | _ ->
                 raise_unexpected_char s c "float_expdig")
      | In_float_exp (ds, d, fs, es, e) ->
          (match c with
             | '0' .. '9' ->
                 s.cursor <- In_float_exp (ds, d, fs, es, 10 * e + dig c)
             | ',' | ']' | '}' ->
                 finish_float_exp s ds d fs es e;
                 parse_char s c
             | _ when is_space c ->
                 update_line_num s c;
                 finish_float_exp s ds d fs es e
             | _ ->
                 raise_unexpected_char s c "float_exp")
      | In_string b ->
          (match c with
             | '\\' ->
                 s.cursor <- In_string_control b
             | '"' ->
                 finish_value s (Json.String (Buffer.contents b))
             | _ ->
                 if is_valid_unescaped_char c then Buffer.add_char b c
                 else raise_unexpected_char s c "string")
      | In_string_control b ->
          (match c with
             | '"' | '\\' | '/' ->
                 Buffer.add_char b c;
                 s.cursor <- In_string b;
             | 'b' ->
                 Buffer.add_char b '\b';
                 s.cursor <- In_string b;
             | 'f' ->
                 Buffer.add_char b '\x0c';
                 s.cursor <- In_string b;
             | 'n' ->
                 Buffer.add_char b '\n';
                 s.cursor <- In_string b;
             | 'r' ->
                 Buffer.add_char b '\r';
                 s.cursor <- In_string b;
             | 't' ->
                 Buffer.add_char b '\t';
                 s.cursor <- In_string b;
             | 'u' ->
                 s.cursor <- In_string_hex (b, [], 4)
             | _ ->
                 raise_unexpected_char s c "string_control")
      | In_string_hex (b, hs, rem) ->
          (if is_hex_char c then begin
             let hs = c :: hs in
               if rem > 1 then
                 s.cursor <- In_string_hex (b, hs, rem - 1)
               else begin
                 (* TODO: We currently just leave the unicode escapes in place. *)
                 Buffer.add_string b (clist_to_string ('\\' :: 'u' :: (List.rev hs)));
                 s.cursor <- In_string b
               end
           end else
             raise_unexpected_char s c "string_unicode")
      | Expect_object_elem_start ->
          (match c with
             | '"' ->
                 s.stack <- (IObject_needs_key []) :: s.stack;
                 s.cursor <- In_string (Buffer.create s.string_buffer_size)
             | '}' ->
                 finish_value s (Json.Object (Array.of_list []))
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "object_start")
      | Expect_object_elem_colon ->
          (match c with
             | ':' ->
                 s.cursor <- Start
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "object_elem_colon")
      | Expect_comma_or_end ->
          (match c with
             | ',' ->
                 if is_parsing_object s then s.cursor <- Expect_object_key
                 else s.cursor <- Expect_value
             | '}' ->
                 if is_parsing_object s then pop_stack s
                 else raise_unexpected_char s c "comma_or_end"
             | ']' ->
                 if not (is_parsing_object s) then pop_stack s
                 else raise_unexpected_char s c "comma_or_end"
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "comma_or_end")
      | Expect_object_key ->
          (match c with
             | '"' ->
                 (match s.stack with
                    | IObject fields :: tl -> s.stack <- IObject_needs_key fields :: tl
                    | io :: _ -> raise_internal_error s ("unexpected " ^ (ivalue_to_str io) ^ " on stack at object_key")
                    | [] -> raise_internal_error s "empty stack at object_key");
                 s.cursor <- In_string (Buffer.create s.string_buffer_size)
             | _ when is_space c ->
                 update_line_num s c
             | _ ->
                 raise_unexpected_char s c "object_key")
      | Done _ ->
          raise_internal_error s "parse called when parse_state is 'Done'"


type parse_result =
  | Json_value of Json.t * (* number of consumed bytes *) int
  | Json_parse_incomplete of parse_state

let parse_substring state str ofs len =
  let i = ref ofs in
  let iend = ofs + len in
    while get_parse_result state = None && !i < iend do
      parse_char state str.[!i];
      (* This is here instead of inside parse_char since
         parse_char makes (tail-)recursive calls without
         consuming a character.
      *)
      state.num_chars_parsed <- state.num_chars_parsed + 1;

      incr i
    done;
    match get_parse_result state with
      | Some v -> Json_value (v, !i - ofs)
      | None -> Json_parse_incomplete state

let parse state str =
  parse_substring state str 0 (String.length str)

(* This is really only required for numbers, since they are only
   terminated by whitespace, but end-of-file or end-of-connection
   qualifies as whitespace.

   The parser might also be just eating whitespace, expecting the
   start of a json value.
*)
let finish_parse state =
  match parse state " " with
    | Json_value (v, _) -> Some v
    | Json_parse_incomplete _ ->
        if state.cursor = Start then None
        else raise_unterminated_value state (current_cursor_value state.cursor)

let num_chars_parsed state = state.num_chars_parsed

(* convenience functions *)
let of_substring str ofs len =
  match parse_substring (init_parse_state ()) str ofs len with
    | Json_value (v, _) ->
        v
    | Json_parse_incomplete st ->
        (match finish_parse st with
           | Some v -> v
           | None -> raise_unterminated_value st (current_cursor_value st.cursor)
        )

let of_string str =
  of_substring str 0 (String.length str)
