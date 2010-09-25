(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010      Prashanth Mundkur.                            *)
(*  Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>          *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public License    *)
(*  as published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or (at your option) any later version.                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

let test_full1 () =
  let s = "http://userinfo@host:80/path?query#fragment" in
  let u = Uri.of_string s in
    assert (u.Uri.scheme = Some "http");
    assert (u.Uri.authority = Some { Uri.userinfo = Some "userinfo";
                                     host = "host";
                                     port = Some 80 });
    assert (u.Uri.path = Some "/path");
    assert (u.Uri.query = Some "query");
    assert (u.Uri.fragment = Some "fragment");
    assert (Uri.abspath_to_string u = "/path?query#fragment");
    assert (Uri.authority_to_string u = "userinfo@host:80");
    assert (Uri.to_string u = s)

let test_partial1 () =
  let s = "http://host:80/path?query" in
  let u = Uri.of_string s in
    assert (u.Uri.scheme = Some "http");
    assert (u.Uri.authority = Some { Uri.userinfo = None;
                                     host = "host";
                                     port = Some 80 });
    assert (u.Uri.path = Some "/path");
    assert (u.Uri.query = Some "query");
    assert (u.Uri.fragment = None);
    assert (Uri.abspath_to_string u = "/path?query");
    assert (Uri.authority_to_string u = "host:80");
    assert (Uri.to_string u = s)

let test_partial2 () =
  let s = "http://host/path#fragment" in
  let u = Uri.of_string s in
    assert (u.Uri.scheme = Some "http");
    assert (u.Uri.authority = Some { Uri.userinfo = None;
                                     host = "host";
                                     port = None });
    assert (u.Uri.path = Some "/path");
    assert (u.Uri.query = None);
    assert (u.Uri.fragment = Some "fragment");
    assert (Uri.abspath_to_string u = "/path#fragment");
    assert (Uri.authority_to_string u = "host");
    assert (Uri.to_string u = s)

let test_partial3 () =
  let s = "http://host" in
  let u = Uri.of_string s in
    assert (u.Uri.scheme = Some "http");
    assert (u.Uri.authority = Some { Uri.userinfo = None;
                                     host = "host";
                                     port = None });
    assert (u.Uri.path = Some "");
    assert (u.Uri.query = None);
    assert (u.Uri.fragment = None);
    assert (Uri.abspath_to_string u = "/");
    assert (Uri.authority_to_string u = "host");
    assert (Uri.to_string u = "http://host/")

let test_partial3 () =
  let s = "file:///path" in
  let u = Uri.of_string s in
    assert (u.Uri.scheme = Some "file");
    assert (u.Uri.authority = None);
    assert (u.Uri.path = Some "/path");
    assert (u.Uri.query = None);
    assert (u.Uri.fragment = None);
    assert (Uri.abspath_to_string u = "/path");
    assert (Uri.authority_to_string u = "");
    assert (Uri.to_string u = "file:///path")

(* TODO: test normalization *)

let _ =
  test_full1 ();
  test_partial1 ();
  test_partial2 ();
  test_partial3 ()
