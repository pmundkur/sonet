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
open Format

exception Unknown_base_type of string

let known_types = ((ref []) : string list ref)
let is_known_type ident = List.mem ident !known_types
let add_known_type ident = known_types := ident :: !known_types
let reset_known_types () = known_types := []

let converters = ((ref []) : (string * string) list ref)
let add_converter ident conv =
  add_known_type ident;
  converters := (ident, conv) :: !converters
let find_converter ident =
  try List.assoc ident !converters
  with Not_found -> ident
let reset_converters () = converters := []

let base_to_str = function
  | B_string -> "Json_conv.string"
  | B_int -> "Json_conv.int"
  | B_int64 -> "Json_conv.int64"
  | B_float -> "Json_conv.float"
  | B_bool -> "Json_conv.bool"
  | B_ident s -> find_converter s

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

  let unqualify n =
    try
      let dindex = String.rindex n '.' in
      let len = String.length n in
        String.sub n (dindex + 1) (len - dindex - 1)
    with Not_found -> n

  let new_ident_from_name env ?(prefix="") ?(suffix="") stem =
    new_var env (prefix ^ unqualify stem ^ suffix)

  let base_to_stem = function
    | B_string -> "str"     | B_int -> "int"        | B_int64 -> "int64"
    | B_bool -> "bool"      | B_float -> "float"    | B_ident s -> unqualify s

  let complex_type_to_stem = function
    | C_base b -> base_to_stem b    | C_option _ -> "opt"   | C_list _ -> "lst"
    | C_array _ -> "arr"            | C_tuple _ -> "tup"    | C_record _ -> "rcd"
    | C_variant _ -> "var"

  let new_ident_from_type env ct =
    new_ident_from_name env (complex_type_to_stem ct)

  let new_idents_from_types env cts =
    let vlist, env =
      List.fold_left (fun (vlist, env) ct ->
                        let v, env' = new_ident_from_type env ct in
                          (v :: vlist), env'
                     ) ([], env) cts in
      (List.rev vlist), env

  let new_ident_from_var env ?(prefix="") ?(suffix="") var =
    new_ident_from_name env ~prefix ~suffix var.stem

  let new_idents_from_vars env ?(prefix="") ?(suffix="") vlist =
    let vlist, env =
      List.fold_left (fun (vlist, env) v ->
                        let v, env' = new_ident_from_var env ~prefix ~suffix v in
                          (v :: vlist), env'
                     ) ([], env) vlist in
      (List.rev vlist), env
end

let verbose = ref false

let dbg fmt =
  let logger s = if !verbose then Printf.printf "%s" s in
    Printf.ksprintf logger fmt

module Pragma = struct
  exception Missing_prefix of string * string
  exception Unknown_pragma of string

  (* NOTE: To make parsing pragmas robust, use a space at each
     end of the Scanf format string.
  *)

  let record_prefix = ref ""
  let try_parse_set_record_prefix p =
    try
      Scanf.sscanf p " set_record_prefix = %s "
        (fun prefix ->
           record_prefix := prefix;
           dbg "Set record prefix to \"%s\".\n" !record_prefix
        );
      true
    with _ -> false

  let try_parse_clear_record_prefix p =
    try
      Scanf.sscanf p " %s "
        (fun p ->
           if p = "clear_record_prefix" then begin
             dbg "Cleared record prefix (was \"%s\").\n" !record_prefix;
             record_prefix := "";
             true
           end else false
        )
    with _ -> false

  let try_parse_use_converter p =
    try
      Scanf.sscanf p " use_converter %s for %s "
        (fun conv typ ->
           dbg "added converter \"%s\" for \"%s\"\n." conv typ;
           add_converter typ conv
        );
      true
    with _ -> false

  let try_parse_open ff p =
    try
      Scanf.sscanf p " open %s "
        (fun m ->
           dbg "inserted open for module \"%s\"\n." m;
           fprintf ff "open %s@\n" m
        );
      true
    with _ -> false

  let clist_to_str clist =
    let len = List.length clist in
    let s = String.create len in
      ignore (List.fold_left (fun pos c -> s.[pos] <- c; (pos - 1)) (len-1) clist);
      s

  let process_pragma ff p =
    let p = clist_to_str p in
      if try_parse_set_record_prefix p then ()
      else if try_parse_clear_record_prefix p then ()
      else if try_parse_use_converter p then ()
      else if try_parse_open ff p then ()
      else raise (Unknown_pragma p)

  let is_prefix prefix str =
    let plen, slen = String.length prefix, String.length str in
      (plen <= slen) && (String.sub str 0 plen = prefix)

  let strip_prefix prefix str =
    let plen, slen = String.length prefix, String.length str in
      if is_prefix prefix str
      then String.sub str plen (slen - plen)
      else str

  let json_field_name fn =
    if is_prefix !record_prefix fn
    then strip_prefix !record_prefix fn
    else raise (Missing_prefix (!record_prefix, fn))
end

type rec_type = First | Next

module To = struct
  let prod_vars_to_str vlist =
    let elems = List.map name_of_var vlist in
      String.concat ", " elems

  let to_array_str ?(constr="") vlist =
    let elems = List.map name_of_var vlist in
    let constr = if constr = "" then "" else "(Json_conv.string_to_json \"" ^ constr ^ "\"); " in
      "[| " ^ constr ^ (String.concat "; " elems) ^ " |]"

  let to_object_str ?(is_record=false) fn_list fv_list =
    let elems = List.map2 (fun f v ->
                             let f = if is_record then Pragma.json_field_name f else f in
                               Printf.sprintf "(\"%s\", %s)" f (name_of_var v)
                          ) fn_list fv_list in
      "[| " ^ (String.concat "; " elems) ^ " |]"

  let to_record_str fnlist fvlist =
    let fields = List.map2 (fun fn fv ->
                              Printf.sprintf "%s = %s" fn (name_of_var fv)
                           ) fnlist fvlist in
      "{ " ^ (String.concat "; " fields) ^ " }"

  let rec to_json ff venv inv typ =
    let v = name_of_var inv in
      match typ with
        | C_base bt ->
            (match bt with
               | B_ident ident -> if not (is_known_type ident) then raise (Unknown_base_type ident)
               | _ -> ());
            fprintf ff "%s_to_json %s" (base_to_str bt) v
        | C_option optt ->
            let optv, venv = Var_env.new_ident_from_type venv optt in
              fprintf ff "(match %s with@," v;
              fprintf ff "| None -> Json.Null@,";
              fprintf ff "@[<v 8>| Some %s ->@," (name_of_var optv);
              to_json ff venv optv optt;
              fprintf ff "@]@,)"
        | C_list elemt ->
            let elemv, venv = Var_env.new_ident_from_type venv elemt in
            let jlistv, venv = Var_env.new_ident_from_name venv v ~suffix:"_jlist" in
            let jlistvn = name_of_var jlistv in
              fprintf ff "@[<v 8>let %s = List.map@," jlistvn;
              fprintf ff "@[<v 8>(fun %s ->@," (name_of_var elemv);
              to_json ff venv elemv elemt;
              fprintf ff "@]@,) %s in@]@," v;
              fprintf ff "Json.Array (Array.of_list %s)" jlistvn
        | C_array elemt ->
            let elemv, venv = Var_env.new_ident_from_type venv elemt in
            let jarrayv, venv = Var_env.new_ident_from_name venv v ~suffix:"_jarray" in
            let jarrayvn = name_of_var jarrayv in
              fprintf ff "@[<v 8>let %s = Array.map@," jarrayvn;
              fprintf ff "@[<v 8>(fun %s ->@," (name_of_var elemv);
              to_json ff venv elemv elemt;
              fprintf ff "@]@,) %s in@]@," v;
              fprintf ff "Json.Array %s" jarrayvn
        | C_tuple ctlist ->
            let cvlist, venv = Var_env.new_idents_from_types venv ctlist in
            let letvlist, venv = Var_env.new_idents_from_vars venv ~prefix:"j_" cvlist in
            let cvtlist = List.combine cvlist ctlist in
              fprintf ff "(match %s with@," v;
              fprintf ff "@[<v 8>| %s ->@," (prod_vars_to_str cvlist);
              List.iter2 (fun letv (cv, ct) ->
                            let_bind ff venv letv cv ct
                         ) letvlist cvtlist;
              fprintf ff "Json.Array %s@]@,)" (to_array_str letvlist)
        | C_record cls ->
            let fnlist, ftlist = List.split cls in
            let fvlist, venv = Var_env.new_idents_from_types venv ftlist in
            let letvlist, venv = Var_env.new_idents_from_vars venv ~prefix:"j_" fvlist in
              fprintf ff "(match %s with@," v;
              fprintf ff "@[<v 8>| %s ->@," (to_record_str fnlist fvlist);
              List.iter2 (fun letv (fv, ft) ->
                            let_bind ff venv letv fv ft
                         ) letvlist (List.combine fvlist ftlist);
              fprintf ff "Json.Object %s@]@,)" (to_object_str ~is_record:true fnlist letvlist)
        | C_variant cdlist ->
            fprintf ff "(match %s with@," v;
            List.iter (fun cd -> variant ff venv cd) cdlist;
            fprintf ff ")"

  and variant ff venv (CD_tuple (vname, vtlist)) =
    let vlist, venv = Var_env.new_idents_from_types venv vtlist in
    let letvlist, venv = Var_env.new_idents_from_vars venv ~prefix:"j_" vlist in
      if List.length vlist = 0 then
        fprintf ff "@[<v 8>| %s ->@," vname
      else
        fprintf ff "@[<v 8>| %s (%s) ->@," vname (prod_vars_to_str vlist);
      List.iter2 (fun letv (v, vt) ->
                    let_bind ff venv letv v vt
                 ) letvlist (List.combine vlist vtlist);
      fprintf ff "Json.Array %s@]@," (to_array_str ~constr:vname letvlist)

  and let_bind ff venv letv inv typ =
    fprintf ff "@[<v 8>let %s =@," (name_of_var letv);
    to_json ff venv inv typ;
    fprintf ff " in@]@,"

  let def ff venv fn_name typ recd =
    let fnv, venv = Var_env.new_ident_from_name venv fn_name in
    let inv, venv = Var_env.new_ident_from_name venv "o" in
    let decl = match recd with First -> "let rec" | Next -> "and" in
      fprintf ff "@[<v 8>%s %s %s =@," decl fn_name (name_of_var inv);
      to_json ff venv inv typ;
      fprintf ff "@]@,@\n@?"
end

module From = struct
  let to_tuple_str ?(constr="") vlist =
    let elems = List.map name_of_var vlist in
    let len = List.length elems in
      (match len with
         | 0 -> Printf.sprintf "%s" constr
         | 1 -> Printf.sprintf "%s %s" constr (List.hd elems)
         | _ -> Printf.sprintf "%s (%s)" constr (String.concat ", " elems))

  let to_record_str fnlist fvlist =
    let fields = List.map2 (fun fn fv ->
                              Printf.sprintf "%s = %s" fn (name_of_var fv)
                           ) fnlist fvlist in
      "{ " ^ (String.concat "; " fields) ^ " }"

  let rec of_json ff venv inv typ tname =
    let v = name_of_var inv in
      match typ with
        | C_base bt ->
            (match bt with
               | B_ident ident -> if not (is_known_type ident) then raise (Unknown_base_type ident)
               | _ -> ());
            fprintf ff "%s_of_json %s" (base_to_str bt) v
        | C_option optt ->
            let optv, venv = Var_env.new_ident_from_type venv optt in
              fprintf ff "(match %s with@," v;
              fprintf ff "| Json.Null -> None@,";
              fprintf ff "@[<v 8>| %s -> @,Some (" (name_of_var optv);
              of_json ff venv optv optt tname;
              fprintf ff ")@]@,)"
        | C_list elemt ->
            let elemv, venv = Var_env.new_ident_from_type venv elemt in
            let oarrayv, venv = Var_env.new_ident_from_name venv v ~suffix:"_oarray" in
            let oarrayvn = name_of_var oarrayv in
              fprintf ff "@[<v 8>let %s = Array.map@," oarrayvn;
              fprintf ff "@[<v 8>(fun %s ->@," (name_of_var elemv);
              of_json ff venv elemv elemt tname;
              fprintf ff "@]@,) (Json_conv.get_array %s) in@]@," v;
              fprintf ff "Array.to_list %s" oarrayvn
        | C_array elemt ->
            let elemv, venv = Var_env.new_ident_from_type venv elemt in
            let oarrayv, venv = Var_env.new_ident_from_name venv v ~suffix:"_oarray" in
            let oarrayvn = name_of_var oarrayv in
              fprintf ff "@[<v 8>let %s = Array.map@," oarrayvn;
              fprintf ff "@[<v 8>(fun %s ->@," (name_of_var elemv);
              of_json ff venv elemv elemt tname;
              fprintf ff "@]@,) (Json_conv.get_array %s) in@]@," v;
              fprintf ff "%s" oarrayvn
        | C_tuple ctlist ->
            let jarrayv, venv = Var_env.new_ident_from_name venv v ~suffix:"_jarray" in
            let jarrayvn = name_of_var jarrayv in
            let letvlist, venv = Var_env.new_idents_from_types venv ctlist in
              fprintf ff "let %s = Json_conv.get_array %s in@," jarrayvn v;
              fprintf ff "Json_conv.check_array_with_length %s %d;@," jarrayvn (List.length ctlist);
              ignore (List.fold_left (fun indx (letv, ct) ->
                                        let inv, venv = Var_env.new_ident_from_name venv "tindx" in
                                          fprintf ff "let %s = %s.(%d) in@," (name_of_var inv) jarrayvn indx;
                                          let_bind ff venv letv inv ct tname;
                                          indx + 1
                                     ) 0 (List.combine letvlist ctlist));
              fprintf ff "%s" (to_tuple_str letvlist)
        | C_record cls ->
            let fnlist, ftlist = List.split cls in
            let letvlist, venv = Var_env.new_idents_from_types venv ftlist in
            let objtv, venv = Var_env.new_ident_from_name venv v ~suffix:"_ftable" in
            let objtvn = name_of_var objtv in
              fprintf ff "let %s = Json_conv.get_object_table %s in@," objtvn v;
              List.iter2 (fun letv (fn, ft) ->
                            let fvar, venv = Var_env.new_ident_from_name venv ~suffix:"_f" fn in
                            let optional = match ft with C_option _ -> "optional_" | _ -> "" in
                              fprintf ff "let %s = Json_conv.get_%sobject_field %s \"%s\" in@," (name_of_var fvar) optional objtvn (Pragma.json_field_name fn);
                              let_bind ff venv letv fvar ft tname
                         ) letvlist cls;
              fprintf ff "%s" (to_record_str fnlist letvlist)
        | C_variant cdlist ->
            let consv, venv = Var_env.new_ident_from_name venv "cons" in
            let consvn = name_of_var consv in
            let argsv, venv = Var_env.new_ident_from_name venv "args" in
            let defmatchv, venv = Var_env.new_ident_from_name venv "s" in
            let defmatchvn = name_of_var defmatchv in
              fprintf ff "let %s, %s = Json_conv.get_variant_constructor %s in@,"
                consvn (name_of_var argsv) v;
              fprintf ff "(match %s with@," consvn;
              List.iter (fun cd -> variant ff venv argsv cd tname) cdlist;
              (* need to write a default match case *)
              fprintf ff "| %s -> Json_conv.raise_unknown_constructor \"%s\" %s@,)"
                defmatchvn tname defmatchvn

  and variant ff venv argsv (CD_tuple (vname, vtlist)) tname =
    let argsvn = name_of_var argsv in
    let vtlen = List.length vtlist in
    let vlist, venv = Var_env.new_idents_from_types venv vtlist in
    let letvlist, venv = Var_env.new_idents_from_vars venv ~prefix:"o_" vlist in
      fprintf ff "@[<v 8>| \"%s\" ->@," vname;
      if vtlen > 0 then
        fprintf ff "Json_conv.check_array_with_length %s %d;@," argsvn (vtlen + 1);
      ignore (List.fold_left (fun indx (letv, vt) ->
                                let inv, venv = Var_env.new_ident_from_name venv "aindx" in
                                  fprintf ff "let %s = %s.(%d) in@," (name_of_var inv) argsvn indx;
                                  let_bind ff venv letv inv vt tname;
                                  indx + 1
                             ) 1 (List.combine letvlist vtlist));
      fprintf ff "%s@]@," (to_tuple_str ~constr:vname letvlist)

  and let_bind ff venv letv inv typ tname =
    fprintf ff "@[<v 8>let %s =@," (name_of_var letv);
    of_json ff venv inv typ tname;
    fprintf ff " in@]@,"

  let def ff venv fn_name (tname, typ) recd =
    let fnv, venv = Var_env.new_ident_from_name venv fn_name in
    let inv, venv = Var_env.new_ident_from_name venv "j" in
    let decl = match recd with First -> "let rec" | Next -> "and" in
      fprintf ff "@[<v 8>%s %s %s =@," decl fn_name (name_of_var inv);
      of_json ff venv inv typ tname;
      fprintf ff "@]@,@\n@?"
end

let generate_to_def ff is_and (tname, trep) =
  To.def ff Var_env.new_env (tname ^ "_to_json") trep is_and

let generate_from_def ff is_and (tname, trep) =
  From.def ff Var_env.new_env (tname ^ "_of_json") (tname, trep) is_and

let generate_header ff ifn =
  let md = Filename.basename (Filename.chop_extension ifn) in
  let argv = Array.to_list Sys.argv in
  let argv = (Filename.basename (List.hd argv)) :: (List.tl argv) in
  let call = String.concat " " argv in
    fprintf ff "(* This file has been auto-generated using \"%s\". *)@\n@\n" call;
    fprintf ff "open %s@\n@\n" (String.capitalize md)

let generate_one_defn ff td =
  match td with
    | [] -> ()
    | h :: t ->
        List.iter (fun (tname, _) -> add_known_type tname) td;
        generate_to_def ff First h;
        List.iter (generate_to_def ff Next) t;
        generate_from_def ff First h;
        List.iter (generate_from_def ff Next) t

let print_exception e =
  match e with
    | Unknown_base_type id ->
        Printf.sprintf "Unknown base type \"%s\"" id
    | Pragma.Missing_prefix (p, f) ->
        Printf.sprintf "Prefix \"%s\" cannot be stripped from field \"%s\"" p f
    | Pragma.Unknown_pragma p ->
        Printf.sprintf "Unable to parse pragma \"%s\"" p
    | e ->
        Printf.sprintf "%s" (Printexc.get_backtrace ())

let generate defn_list ofn ifn =
  reset_known_types ();
  reset_converters ();
  (try Unix.unlink ofn with _ -> ());
  let op_flags = [ Open_wronly ; Open_creat; Open_trunc; Open_text ] in
  let oc = open_out_gen op_flags 0o444 ofn in
  let ff = formatter_of_out_channel oc in
    try
      generate_header ff ifn;
      List.iter (function
                   | Pragma p ->
                       Pragma.process_pragma ff p
                   | Type_defn (T_manifest def) ->
                       generate_one_defn ff def
                   | Type_defn (T_abstract tname) ->
                       ()
                ) defn_list;
      close_out oc
    with e ->
      Printf.eprintf "Error: %s\n" (print_exception e);
      close_out oc;
      Unix.unlink ofn
