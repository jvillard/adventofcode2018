open Core

let test_input = {|#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2|}

type claim = {id: int; pad_left: int; pad_top: int; width: int; height: int}
[@@deriving sexp]

type suqare = Empty | One of int | TwoOrMore

let bump suit id i j =
  match suit.(i).(j) with
  | Empty -> suit.(i).(j) <- One id
  | One _ -> suit.(i).(j) <- TwoOrMore
  | TwoOrMore -> ()

let iter_claim_squares suit claim ~f =
  for i = claim.pad_left to claim.pad_left + claim.width - 1 do
    for j = claim.pad_top to claim.pad_top + claim.height - 1 do
      f suit claim.id i j
    done
  done

let suit_size = 1200

let register_claims claims =
  let suit = Array.make_matrix ~dimx:suit_size ~dimy:suit_size Empty in
  List.iter claims ~f:(fun claim -> iter_claim_squares suit claim ~f:bump) ;
  suit

let pp_suit f suit =
  Array.iter suit ~f:(fun line ->
      Array.iter line ~f:(function
        | Empty -> Format.pp_print_char f '.'
        | One _ -> Format.pp_print_char f 'x'
        | TwoOrMore -> Format.pp_print_char f '#' ) ;
      Format.fprintf f "\n" )

let count_overlapping_squares suit =
  (* Format.eprintf "%a@." pp_suit suit ; *)
  let count = ref 0 in
  Array.iter suit
    ~f:(Array.iter ~f:(function TwoOrMore -> incr count | _ -> ())) ;
  !count

let find_non_overlapping_claim suit claims =
  let exception Nope in
  let square_belongs_to_claim suit id i j =
    match suit.(i).(j) with One id' when id = id' -> () | _ -> raise Nope
  in
  List.find_exn claims ~f:(fun claim ->
      try
        iter_claim_squares suit claim ~f:square_belongs_to_claim ;
        true
      with Nope -> false )

let claims_of_lines lines =
  let rec aux claims = function
    | [] -> List.rev claims
    | line :: lines ->
        let claim =
          Scanf.sscanf line "#%d @ %d,%d: %dx%d"
            (fun id pad_left pad_top width height ->
              {id; pad_left; pad_top; width; height} )
        in
        aux (claim :: claims) lines
  in
  aux [] lines

let input = In_channel.read_lines "suit_3.txt" |> claims_of_lines

let test () =
  let claims = String.split ~on:'\n' test_input |> claims_of_lines in
  let suit = register_claims claims in
  let part1_result = count_overlapping_squares suit in
  if part1_result <> 4 then (
    Format.eprintf "huho, got %d for the test @." part1_result ;
    exit 1 ) ;
  let part2_result =
    find_non_overlapping_claim suit claims |> fun {id} -> id
  in
  if part2_result <> 3 then (
    Format.eprintf "huho, got %d for the test @." part2_result ;
    exit 1 )

let () =
  let suit = register_claims input in
  test () ;
  Format.printf "part 1: %d@." (count_overlapping_squares suit) ;
  Format.printf "part 2: %d@."
    (find_non_overlapping_claim suit input |> fun {id} -> id) ;
  ()
