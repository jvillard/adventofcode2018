open Core

module Distance = struct
  type int_ = int [@@deriving sexp]

  let compare_int_ _ _ = 0

  type t = {point_id: int_ option; distance: int} [@@deriving compare, sexp]
end

type point = {id: int; x: int; y: int} [@@deriving sexp]

let () =
  sexp_of_point {id= 3; x= 5; y= 5}
  |> point_of_sexp |> sexp_of_point |> Sexp.to_string |> print_endline x

let pp_matrix pp f map =
  Array.iter map ~f:(fun line ->
      Array.iter line ~f:(pp f) ;
      Format.fprintf f "\n" )

let pp_map1 =
  pp_matrix (fun f -> function
    | None -> Format.pp_print_string f " . "
    | Some {Distance.point_id= Some id} -> Format.fprintf f "%2d " id
    | Some {point_id= None} -> Format.pp_print_string f " # " )

let fill_distance_map_for_point (north, south, east, west) map point =
  for i = west to east do
    for j = north to south do
      let distance = abs (point.x - i) + abs (point.y - j) in
      match map.(i).(j) with
      | Some {Distance.point_id= _; distance= min_distance}
        when min_distance = distance ->
          map.(i).(j) <- Some {Distance.point_id= None; distance}
      | Some {Distance.point_id= _; distance= min_distance}
        when min_distance < distance ->
          ()
      | _ -> map.(i).(j) <- Some {point_id= Some point.id; distance}
      | exception Invalid_argument _ ->
          failwith (Printf.sprintf "failed to access %d, %d" i j)
    done
  done

let find_edges points =
  let north, south, east, west =
    List.fold points ~init:(None, None, None, None)
      ~f:(fun (north, south, east, west) point ->
        let north =
          match north with
          | Some y when y <= point.y -> north
          | _ -> Some point.y
        in
        let south =
          match south with
          | Some y when y >= point.y -> south
          | _ -> Some point.y
        in
        let east =
          match east with
          | Some x when x >= point.x -> east
          | _ -> Some point.x
        in
        let west =
          match west with
          | Some x when x <= point.x -> west
          | _ -> Some point.x
        in
        (north, south, east, west) )
  in
  match Option.all [north; south; east; west] with
  | Some [north; south; east; west] -> (north, south, east, west)
  | _ -> failwith "couldn't find edges"

let make_distance_map points =
  let ((_, south, east, _) as edges) = find_edges points in
  let map = Array.make_matrix ~dimx:(east + 1) ~dimy:(south + 1) None in
  List.iter points ~f:(fill_distance_map_for_point edges map) ;
  (* Format.eprintf "Result:@\n%a@." pp_map map ; *)
  (edges, map)

let part1 points =
  let (north, south, east, west), map = make_distance_map points in
  let covered_by =
    Array.foldi map ~init:[] ~f:(fun i covered_by ->
        Array.foldi ~init:covered_by ~f:(fun j covered_by distance ->
            match distance with
            | Some {Distance.point_id= Some id} ->
                let old_coverage =
                  List.Assoc.find ~equal:Int.equal covered_by id
                  |> Option.value ~default:0
                in
                List.Assoc.add ~equal:Int.equal covered_by id (old_coverage + 1)
            | _ -> covered_by ) )
  in
  let infinite_areas = ref Int.Set.empty in
  for i = west to east do
    ( match map.(i).(north) with
    | Some {Distance.point_id= Some id} ->
        infinite_areas := Int.Set.add !infinite_areas id
    | _ -> () ) ;
    match map.(i).(south) with
    | Some {Distance.point_id= Some id} ->
        infinite_areas := Int.Set.add !infinite_areas id
    | _ -> ()
  done ;
  for j = north to south do
    ( match map.(west).(j) with
    | Some {Distance.point_id= Some id} ->
        infinite_areas := Int.Set.add !infinite_areas id
    | _ -> () ) ;
    match map.(east).(j) with
    | Some {Distance.point_id= Some id} ->
        infinite_areas := Int.Set.add !infinite_areas id
    | _ -> ()
  done ;
  let infinite_areas = !infinite_areas in
  Format.eprintf "bergl: %a@\ninfinite: %a@." Sexp.pp
    ((List.Assoc.sexp_of_t Int.sexp_of_t Int.sexp_of_t) covered_by)
    Sexp.pp
    (Int.Set.sexp_of_t infinite_areas) ;
  let covered_by =
    List.filter covered_by ~f:(fun (id, _) ->
        not (Int.Set.mem infinite_areas id) )
  in
  Format.eprintf "filtered bergl: %a@." Sexp.pp
    ((List.Assoc.sexp_of_t Int.sexp_of_t Int.sexp_of_t) covered_by) ;
  List.fold covered_by ~init:(-1) ~f:(fun max_coverage (id, coverage) ->
      max max_coverage coverage )

let pp_map2 = pp_matrix (fun f sum -> Format.fprintf f "%3d" sum)

let fill_distance_sum_for_point map point =
  Array.mapi map ~f:(fun i ->
      Array.mapi ~f:(fun j sum ->
          let distance = abs (point.x - i) + abs (point.y - j) in
          sum + distance ) )

let part2 points target =
  let _, south, east, _ = find_edges points in
  let map = Array.make_matrix ~dimx:(east + target) ~dimy:(south + target) 0 in
  let map = List.fold points ~init:map ~f:fill_distance_sum_for_point in
  (* Format.eprintf "debug2:@\n%a@." pp_map2 map ; *)
  Array.fold ~init:0 map ~f:(fun count ->
      Array.fold ~init:count ~f:(fun count total_distance ->
          if total_distance < target then count + 1 else count ) )

let point_of_line id line = Scanf.sscanf line "%d, %d" (fun x y -> {id; x; y})

let test_input =
  {|1, 1
1, 6
8, 3
3, 4
5, 5
8, 9|} |> String.split_lines
  |> List.mapi ~f:point_of_line

let input = In_channel.read_lines "coord_6.txt" |> List.mapi ~f:point_of_line

let assert_equal pp err x y =
  if x <> y then failwith (Format.asprintf "ERROR: %s: %a <> %a" err pp x pp y)

let test () =
  let points = test_input in
  assert_equal Int.pp "part 1" (part1 points) 17 ;
  assert_equal Int.pp "part 2" (part2 points 32) 16

let () =
  test () ;
  Format.printf "Part 1: %d@." (part1 input) ;
  Format.printf "Part 2: %d@." (part2 input 10_000)
