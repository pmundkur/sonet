(* Taken from:
   https://github.com/cheecheeo/JSON-shootout/blob/90f465a70dd3814a0e9ad7b661684fac0bfbd726/ocaml_json.ml
*)
module Y = Yojson

let id (x : 'a) : 'a = x

let channel_get_lines (channel : in_channel) : string list =
  let rec get_lines_acc (return : string list -> string list)
                        (channel : in_channel)
                        : string list =
    let x = try Some (input_line channel) with End_of_file -> None
    in match x with
      | None -> return []
      | Some h -> get_lines_acc (fun result -> return (h :: result)) channel
  in get_lines_acc id channel

let () =
  let ss = String.concat "\n" (channel_get_lines stdin) in
  let t0 = Unix.gettimeofday () in
  let json = Y.Basic.from_string ss in
  let t1 = Unix.gettimeofday ()
  in begin
    print_endline (Y.Basic.to_string json);
    print_endline (string_of_float (t1 -. t0))
  end
