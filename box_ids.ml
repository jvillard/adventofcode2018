open Core

let input =
  In_channel.with_file "box_ids.txt" ~f:(fun chan ->
      In_channel.input_lines chan )

let box_id_has_2_or_3_identical_letters box_id =
  let counts =
    String.fold box_id ~init:[] ~f:(fun counts c ->
        let count =
          List.Assoc.find ~equal:Char.equal counts c |> Option.value ~default:0
        in
        List.Assoc.add ~equal:Char.equal counts c (count + 1) )
  in
  let has2 = List.exists counts ~f:(fun (_, n) -> Int.equal n 2) in
  let has3 = List.exists counts ~f:(fun (_, n) -> Int.equal n 3) in
  (has2, has3)

let boxes_with_2_and_3_identical_letters input =
  let rec aux (count2, count3) = function
    | [] -> count2 * count3
    | box_id :: rest ->
        let has2, has3 = box_id_has_2_or_3_identical_letters box_id in
        let count2 = if has2 then count2 + 1 else count2 in
        let count3 = if has3 then count3 + 1 else count3 in
        aux (count2, count3) rest
  in
  aux (0, 0) input

let find_pos_where_different box_id1 box_id2 len =
  let pos_where_different = ref None in
  try
    for i = 0 to len do
      if
        not
          (Char.equal
             (String.unsafe_get box_id1 i)
             (String.unsafe_get box_id2 i))
      then
        if Option.is_some !pos_where_different then (
          pos_where_different := None ;
          raise (Not_found_s (Sexp.Atom "too many differences")) )
        else pos_where_different := Some i
    done ;
    !pos_where_different
  with Not_found_s _ -> None

let find_different_by_one_letter input =
  let find_for_one_pair box_id1 box_id2 =
    let len = String.length box_id1 in
    assert (Int.equal len (String.length box_id2)) ;
    find_pos_where_different box_id1 box_id2 len
    |> Option.map ~f:(fun i ->
           String.sub ~pos:0 ~len:i box_id1
           ^ String.sub ~pos:(i + 1) ~len:(len - i - 1) box_id1 )
  in
  List.find_map_exn input ~f:(fun box_id ->
      List.find_map input ~f:(find_for_one_pair box_id) )

let part2_test () =
  let input =
    ["abcde"; "fghij"; "klmno"; "pqrst"; "fguij"; "axcye"; "wvxyz"]
  in
  assert (String.equal (find_different_by_one_letter input) "fgij")

let () =
  Format.printf "part 1 result=%d@."
    (boxes_with_2_and_3_identical_letters input) ;
  Format.printf "part 2 result=%s@." (find_different_by_one_letter input) ;
  part2_test () ;
  ()
