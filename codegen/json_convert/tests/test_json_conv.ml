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

open Test_types
open Test_types_json_conv

let do_print = ref false

let test_list to_j of_j o_list =
  List.iter (fun t ->
               let j = to_j t in
               let o = of_j j in
                 if !do_print then
                   Printf.printf "testing of_j(to_j(.) == . for %s\n" (Json.to_string j);
                 assert (o = t)
            ) o_list;
  let j_list = List.map to_j o_list in
    List.iter (fun t ->
                 let o = of_j t in
                 let j = to_j o in
                   if !do_print then
                     Printf.printf "testing to_j(of_j(.) == . for %s\n" (Json.to_string j);
                   assert (j = t)
              ) j_list


let check_base_type () =
  let bs = [ B_int 3;
             B_int64 1L;
             B_bool false;
             B_string "test";
             B_float 0.1
           ] in
    test_list base_type_to_json base_type_of_json bs

let check_simple_type () =
  let ss = [ S_int_option None;
             S_int_option (Some 2);
             S_int64_option (Some 0L);
             S_bool_option (Some true);
             S_string_option (Some "tset");
             S_float_option (Some 0.1);

             S_int_list [ ];
             S_int_list [ 3; 2; -1 ];
             S_bool_list [ true; false; false; true; false ];
             S_int64_list [ 1L; -3L; 2L; 5L];
             S_string_list [ "iggy"; "franti"; "zappa" ];

             S_int_array [| |];
             S_int_array [| 1; 3; 2 |];
             S_bool_array [| false; true; false; false; true |];
             S_int64_array [| 1L; 3L; -2L; 5L |];
             S_string_array [| "iggy"; "franti"; "zappa" |]
           ] in
    test_list simple_type_to_json simple_type_of_json ss

let check_record_type () =
  let rs = [ { record_int = 32;
               record_int64 = 32L;
               record_bool = false;
               record_string = "record";

               record_int_list = [ 0; 1; 2; -6; 4; 5];
               record_int64_option_array = [| Some 0L; Some (-3L); None; Some (-1L); Some 5L |];
               record_bool_array = [| false; true; false; false; true |];

               record_prod_list = [ (1,false); (-23, true); (-1000, true) ], "prod"
             } ] in
    test_list record_type_to_json record_type_of_json rs

let check_complex_type1 () =
  let cs = [ [| |];
             [| ([], true) |];
             [| ([4; 3; 1], false); ([1; 3; 4], true) |];
           ] in
    test_list complex_type1_to_json complex_type1_of_json cs

let check_complex_type2 () =
  let cs = [ { record = { record_int = 32;
                          record_int64 = 32L;
                          record_bool = false;
                          record_string = "record";

                          record_int_list = [ 0; 1; 2; -6; 4; 5];
                          record_int64_option_array = [| Some 0L; Some (-3L); None; Some (-1L); Some 5L |];
                          record_bool_array = [| false; true; false; false; true |];

                          record_prod_list = [ (1,false); (-23, true); (-1000, true) ], "prod"
                        };
               complex_type1 = [| ([4; 3; 1], false); ([1; 3; 4], true) |];
             }
           ] in
    test_list complex_type2_to_json complex_type2_of_json cs

let check_open_and_conv () =
  let cs = [ 1L; -1L ] in
    test_list t_to_json t_of_json cs

let parse_args () =
  let options = [("-print-value", Arg.Set do_print, " print output")] in
  let usage = Printf.sprintf "Usage: %s [options]" Sys.argv.(0) in
    Arg.parse (Arg.align options) (fun f -> ()) usage

let _ =
  parse_args ();
  check_base_type ();
  check_simple_type ();
  check_record_type ();
  check_complex_type1 ();
  check_complex_type2 ();
  check_open_and_conv ()

