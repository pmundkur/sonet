(*
 * Copyright (C) 2009      Citrix Ltd.
 * Author Prashanth Mundkur <firstname.lastname@citrix.com>
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

type t =
  | Null
  | Undefined
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Object of (string * t) array
  | Array of t array

let string_of_type t =
  match t with
    | Null      -> "null"
    | Undefined -> "undefined"
    | Bool _    -> "bool"
    | Int _     -> "int"
    | Float _   -> "float"
    | String _  -> "string"
    | Array _   -> "array"
    | Object _  -> "object"

let escape_string s =
  let buf = Buffer.create 80 in
    Buffer.add_char buf '"';
    for i = 0 to String.length s - 1 do
      let x =
        match s.[i] with
          | '\n'   -> "\\n"
          | '\t'   -> "\\t"
          | '\r'   -> "\\r"
          | '\b'   -> "\\b"
          | '\\'   -> "\\\\"
          | '/'    -> "\\/"
          | '"'    -> "\\\""
          | '\x0c' -> "\\f"
          | c      -> String.make 1 c
      in
        Buffer.add_string buf x
    done;
    Buffer.add_char buf '"';
    Buffer.contents buf

let rec list_iter_between f o a =
  let len = Array.length a in
    Array.iteri (fun i v -> f v; if i < len - 1 then o ()) a


let rec to_fct t f =
  match t with
    | Int i      -> f (Printf.sprintf "%Ld" i)
    | Float r    -> f (Printf.sprintf "%f" r)
    | String s   -> f (escape_string s)
    | Bool b     -> f (string_of_bool b)
    | Undefined  -> f "undefined"
    | Null       -> f "null"
    | Array a    ->
        f "[";
        list_iter_between (fun i -> to_fct i f) (fun () -> f ", ") a;
        f "]";
    | Object a   ->
        f "{";
        list_iter_between (fun (k, v) -> to_fct (String k) f; f ": "; to_fct v f)
          (fun () -> f ", ") a;
        f "}"

let to_fct_pretty t f =
  let il = ref 0 in
  let had_newline = ref true in

  let nl () = f "\n"; had_newline := true in
  let pi () = if !had_newline then (f (String.make (2 * !il) ' '); had_newline := false) in

  let rec process t =
    pi ();
    match t with
      | Int i      -> f (Printf.sprintf "%Ld" i)
      | Float r    -> f (Printf.sprintf "%f" r)
      | String s   -> f (escape_string s)
      | Bool b     -> f (string_of_bool b)
      | Undefined  -> f "undefined"
      | Null       -> f "null"
      | Array a    ->
          if Array.length a = 0 then
            f "[]"
          else (
            f "["; nl (); incr il;
            list_iter_between (fun i -> pi (); process i) (fun () -> f ","; nl ()) a;
            nl (); decr il; pi (); f "]";
          )
      | Object a   ->
          if Array.length a = 0 then
            f "{ }"
          else (
            f "{"; nl (); incr il;
            list_iter_between (fun (k, v) -> process (String k); f ": "; process v)
              (fun () -> f ","; nl()) a;
            nl (); decr il; pi (); f "}"
          )
  in
    process t;
    nl ()

let to_buffer t buf =
  to_fct t (fun s -> Buffer.add_string buf s)

let to_string t =
  let buf = Buffer.create 2048 in
    to_buffer t buf;
    Buffer.contents buf

let is_null = function Null -> true | _ -> false
let is_bool = function Bool _ -> true | _ -> false
let is_int  = function Int _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false
let is_string = function String _ -> true | _ -> false
let is_object = function Object _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_scalar j = not (is_object j) && not (is_array j)
