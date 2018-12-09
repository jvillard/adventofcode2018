open! Core

type timestamp = {year: int; month: int; day: int; hour: int; minute: int}
[@@deriving compare]

type entry =
  | BeginShift of timestamp * int
  | Sleep of timestamp
  | Rise of timestamp

let get_timestamp = function
  | BeginShift (timestamp, _) | Sleep timestamp | Rise timestamp -> timestamp

let compare_entry entry1 entry2 =
  compare_timestamp (get_timestamp entry1) (get_timestamp entry2)

let log_guard_minutes log =
  let rec aux (guard_sleeps, cur_guard) = function
    | BeginShift (_, id) :: tl -> aux (guard_sleeps, id) tl
    | Sleep {minute= sleep} :: Rise {minute= rise} :: tl when cur_guard >= 0 ->
        let guard_sleeps =
          Int.Map.change guard_sleeps cur_guard ~f:(fun times_opt ->
              let sleep_times =
                match times_opt with
                | None -> Array.create ~len:60 0
                | Some times -> times
              in
              for i = sleep to rise - 1 do
                sleep_times.(i) <- sleep_times.(i) + 1
              done ;
              Some sleep_times )
        in
        aux (guard_sleeps, cur_guard) tl
    | [] -> guard_sleeps
    | _ -> assert false
  in
  aux (Int.Map.empty, -12) log

(* part 1 *)
let find_guard_and_minute log =
  let guard_sleeps = log_guard_minutes log in
  let one_guard times =
    Array.foldi times ~init:(0, -1, -1)
      ~f:(fun minute (total, max, max_minute) count ->
        let total = total + count in
        if count > max then (total, count, minute) else (total, max, max_minute)
    )
  in
  let _cur_max, max_minute, guard_id =
    Int.Map.fold guard_sleeps ~init:(0, 0, 0)
      ~f:(fun ~key:guard ~data:times ((cur_max, _, _) as result) ->
        let total_slept, _, max_minute = one_guard times in
        if total_slept > cur_max then (total_slept, max_minute, guard)
        else result )
  in
  max_minute * guard_id

(* part 2 *)
let find_max_minute_and_guard log =
  let guard_sleeps = log_guard_minutes log in
  let one_guard times =
    Array.foldi times ~init:(0, -1) ~f:(fun minute (max, max_minute) count ->
        if count > max then (count, minute) else (max, max_minute) )
  in
  let _, max_minute, guard_id =
    Int.Map.fold guard_sleeps ~init:(0, -1, -1)
      ~f:(fun ~key:guard ~data:times ((cur_max, _, _) as result) ->
        let time, max_minute = one_guard times in
        if time > cur_max then (time, max_minute, guard) else result )
  in
  max_minute * guard_id

let entries_of_lines lines =
  let rec aux entries = function
    | [] -> List.rev entries
    | line :: lines -> (
        let action =
          String.sub ~pos:(String.length "[YYYY-MM-DD HH:MM] ") ~len:5 line
        in
        match action with
        | "Guard" ->
            Scanf.sscanf line "[%d-%d-%d %d:%d] Guard #%d begins shift"
              (fun year month day hour minute id ->
                aux
                  (BeginShift ({year; month; day; hour; minute}, id) :: entries)
                  lines )
        | "falls" ->
            Scanf.sscanf line "[%d-%d-%d %d:%d]"
              (fun year month day hour minute ->
                aux (Sleep {year; month; day; hour; minute} :: entries) lines
            )
        | "wakes" ->
            Scanf.sscanf line "[%d-%d-%d %d:%d]"
              (fun year month day hour minute ->
                aux (Rise {year; month; day; hour; minute} :: entries) lines )
        | _ -> failwith ("Unexpected action " ^ action) )
  in
  aux [] lines |> List.sort ~compare:compare_entry

let test_input =
  {|[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up|}

let assert_equal pp err x y =
  if x <> y then failwith (Format.asprintf "ERROR: %s: %a <> %a" err pp x pp y)

let test () =
  let part1_result =
    String.split_lines test_input |> entries_of_lines |> find_guard_and_minute
  in
  assert_equal Int.pp "part 1" part1_result 240 ;
  let part2_result =
    String.split_lines test_input
    |> entries_of_lines |> find_max_minute_and_guard
  in
  assert_equal Int.pp "part 2" part2_result 4455

let () =
  test () ;
  Format.printf "Part 1: %d@."
    ( In_channel.read_lines "guards_4.txt"
    |> entries_of_lines |> find_guard_and_minute ) ;
  Format.printf "Part 2: %d@."
    ( In_channel.read_lines "guards_4.txt"
    |> entries_of_lines |> find_max_minute_and_guard )
