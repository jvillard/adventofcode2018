open Core
module Steps = Set.Make (Char)
module Graph = Map.Make (Char)

let debug = false

let log fmt =
  if debug then Format.eprintf fmt
  else Format.ifprintf Format.std_formatter fmt

let find_available g =
  Graph.fold g ~init:Steps.empty ~f:(fun ~key:step ~data:deps available ->
      if Steps.is_empty deps then Steps.add available step else available )

let do_step g step =
  let g = Graph.map g ~f:(fun deps -> Steps.remove deps step) in
  Graph.remove g step

let part1 dep_g =
  let rec build_order g rev_order =
    match find_available g |> Steps.to_list with
    | [] ->
        (* Format.eprintf "graph:%a@." Sexp.pp (Graph.sexp_of_t Steps.sexp_of_t g) ; *)
        assert (Graph.for_all g ~f:Steps.is_empty) ;
        List.rev rev_order
    | available_step :: _ ->
        let g = do_step g available_step in
        build_order g (available_step :: rev_order)
  in
  build_order dep_g [] |> String.of_char_list

let cost_of_step ~step_overhead step =
  Char.to_int step - Char.to_int 'A' + 1 + step_overhead

let wait_until_one_free ~allow_zero workers =
  let min =
    List.fold workers ~init:None ~f:(fun acc (t, _) ->
        if allow_zero || t <> 0 then
          match acc with Some acc -> Some (min t acc) | None -> Some t
        else acc )
  in
  let rec sub t new_workers = function
    | [] -> (* don't care about the order *) new_workers
    | (t_w, step_w) :: workers ->
        let new_w = (max (t_w - t) 0, step_w) in
        sub t (new_w :: new_workers) workers
  in
  match min with
  | Some 0 | None -> None
  | Some min -> Some (min, sub min [] workers)

let assign_step ~step_overhead workers step =
  let rec aux step new_workers = function
    | [] -> assert false
    | (0, None) :: workers ->
        (cost_of_step ~step_overhead step, Some step)
        :: List.rev_append new_workers workers
    | worker :: workers -> aux step (worker :: new_workers) workers
  in
  aux step [] workers

let reap_done_steps workers =
  let rec aux steps_done new_workers = function
    | [] -> (steps_done, new_workers)
    | (0, Some step) :: workers ->
        aux (step :: steps_done) ((0, None) :: new_workers) workers
    | worker :: workers -> aux steps_done (worker :: new_workers) workers
  in
  aux [] [] workers

let part2 ~step_overhead ~jobs dep_g =
  (* set of "time before worker is available again" *)
  let workers = List.init jobs ~f:(fun _ -> (0, None)) in
  let rec do_all_tasks workers g time =
    let time_waited, workers =
      wait_until_one_free ~allow_zero:true workers
      |> Option.value ~default:(0, workers)
    in
    log "waited %d@\n" time_waited ;
    let steps_done, workers = reap_done_steps workers in
    log "finished steps: %a@\n"
      (Format.pp_print_list Format.pp_print_char)
      steps_done ;
    let g = List.fold ~init:g ~f:do_step steps_done in
    let time = time + time_waited in
    match find_available g |> Steps.to_list with
    | [] -> (
      match wait_until_one_free ~allow_zero:false workers with
      | Some (time_waited, workers) ->
          log "WAITED %d@\n" time_waited ;
          let time = time + time_waited in
          do_all_tasks workers g time
      | None ->
        (* all workers are done, so since we reaped the done tasks already and there are no
           available steps then it must be that we are done *)
          log "graph:%a@." Sexp.pp (Graph.sexp_of_t Steps.sexp_of_t g) ;
          assert (Graph.for_all g ~f:Steps.is_empty) ;
          let time_to_finish =
            List.fold workers ~init:0 ~f:(fun time (t_w, _) -> max time t_w)
          in
          time + time_to_finish )
    | available_step :: _ ->
        log "assigning step: %c@\n" available_step ;
        let g = Graph.remove g available_step in
        let workers = assign_step ~step_overhead workers available_step in
        do_all_tasks workers g time
  in
  do_all_tasks workers dep_g 0

let make_graph constraints =
  List.fold constraints ~init:Graph.empty ~f:(fun g (dep, step) ->
      (* create node if not there *)
      let g =
        Graph.update g dep ~f:(function
          | None -> Steps.empty
          | Some deps -> deps )
      in
      Graph.update g step ~f:(function
        | Some deps -> Steps.add deps dep
        | None -> Steps.singleton dep ) )

let constraints_of_lines lines =
  List.map lines ~f:(fun line ->
      Scanf.sscanf line "Step %c must be finished before step %c can begin."
        (fun dep step -> (dep, step) ) )
  |> make_graph

let test_input =
  {|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.|}
  |> String.split_lines |> constraints_of_lines

let assert_equal pp err x y =
  if x <> y then failwith (Format.asprintf "ERROR: %s: %a <> %a" err pp x pp y)

let test () =
  assert_equal String.pp "part 1" (part1 test_input) "CABDFE" ;
  assert_equal Int.pp "part 2" (part2 ~step_overhead:0 ~jobs:2 test_input) 15

let input = In_channel.read_lines "schedule_7.txt" |> constraints_of_lines

let () =
  test () ;
  Format.printf "part 1=%s@." (part1 input) ;
  Format.printf "part 2=%d@." (part2 ~step_overhead:60 ~jobs:5 input) ;
  ()
