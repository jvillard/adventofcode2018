open Core

type polarity = Lower | Upper

let opposite_polarities p1 p2 = p1 <> p2

module Unit : sig
  type t = private {id: char; polarity: polarity}

  val make : id:Char.t -> polarity:polarity -> t
end = struct
  type t = {id: char; polarity: polarity}

  let make ~id ~polarity =
    assert ('a' <= id && id <= 'z') ;
    {id; polarity}
end

let can_react unit1 unit2 =
  Char.equal unit1.Unit.id unit2.Unit.id
  && opposite_polarities unit1.polarity unit2.polarity

let reduce polymer =
  let rec aux rev_prefix rest =
    match (rest, rev_prefix) with
    | [], _ -> rev_prefix
    | p :: rest', r :: rev_prefix' when can_react p r -> aux rev_prefix' rest'
    | p :: rest', _ -> aux (p :: rev_prefix) rest'
  in
  aux [] polymer

let part1 polymer = reduce polymer |> List.length

let part2 polymer =
  let erase_from_poly c polymer =
    List.filter polymer ~f:(fun {Unit.id} -> not (Char.equal id c))
  in
  let all_lowercase =
    let a = Char.to_int 'a' in
    Array.init 26 ~f:(fun i -> Char.unsafe_of_int (a + i)) |> Array.to_list
  in
  List.fold all_lowercase
    ~init:(List.length polymer + 1)
    ~f:(fun min_length c ->
      let len = part1 (erase_from_poly c polymer) in
      min len min_length )

let polymer_of_string s =
  String.fold ~init:[] s ~f:(fun polymer c ->
      let unit =
        if Char.is_lowercase c then Unit.make ~id:c ~polarity:Lower
        else Unit.make ~id:(Char.lowercase c) ~polarity:Upper
      in
      unit :: polymer )
  |> List.rev

let input =
  match In_channel.read_lines "polymers_5.txt" with
  | [line] -> polymer_of_string line
  | lines ->
      failwith
        ("expected 1 line of input, got " ^ string_of_int (List.length lines))

let assert_equal pp err x y =
  if x <> y then failwith (Format.asprintf "ERROR: %s: %a <> %a" err pp x pp y)

let test () =
  let polymer = polymer_of_string "dabAcCaCBAcCcaDA" in
  assert_equal Int.pp "part 1" (part1 polymer) 10 ;
  assert_equal Int.pp "part 2" (part2 polymer) 4

let () =
  test () ;
  Format.printf "Part 1: %d@." (part1 input) ;
  Format.printf "Part 2: %d@." (part2 input)
