open Board

(** Module constants for better maintainability *)
module Constants = struct
  let board_size = 9
  let box_size = 3
  let total_cells = 81
  let pool_size = 10
  let diagonal_positions = [(0, 0); (3, 3); (6, 6)]
end

(** Puzzle difficulty levels with associated cell removal targets *)
type difficulty =
  | Easy
  | Medium
  | Hard

(** Configuration for difficulty levels *)
let difficulty_config = function
  | Easy -> 35 (* ~57% filled *)
  | Medium -> 45 (* ~44% filled *)
  | Hard -> 55 (* ~32% filled *)

(** Error types for better error handling *)
type generation_error =
  | Invalid_diagonal_position of int * int
  | Board_solving_failed
  | Invalid_board_state

(** Result type for operations that can fail *)
type 'a result = ('a, generation_error) Result.t

(** Fisher-Yates shuffle utilities *)
module Fisher_yates = struct
  let shuffle_inplace ?(rng = Random.State.make_self_init ()) arr =
    let len = Array.length arr in
    for i = len - 1 downto 1 do
      let j = Random.State.int rng (i + 1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(j) ;
      arr.(j) <- temp
    done

  let shuffle ?(rng = Random.State.make_self_init ()) arr =
    let arr_copy = Array.copy arr in
    shuffle_inplace ~rng arr_copy ;
    arr_copy
end

(** Immutable shuffle pool to avoid global mutable state *)
module ShufflePool = struct
  type t = int array array

  (** Create a new shuffle pool with pre-shuffled arrays *)
  let create ?(rng = Random.State.make_self_init ()) () : t =
    let pool = Array.make Constants.pool_size [||] in
    for i = 0 to Constants.pool_size - 1 do
      let nums = Array.init Constants.board_size (fun j -> j + 1) in
      Fisher_yates.shuffle_inplace ~rng nums ;
      pool.(i) <- nums
    done ;
    pool

  (** Get a shuffled array from the pool, cycling through indices *)
  let get_shuffled pool ~index ~rng =
    let pool_idx = index mod Constants.pool_size in
    let nums = Array.copy pool.(pool_idx) in
    Fisher_yates.shuffle_inplace ~rng nums ;
    nums
end

(** Board utilities *)
module BoardUtils = struct
  (** Validate diagonal box position *)
  let is_valid_diagonal_pos row col =
    List.mem row [0; 3; 6] && List.mem col [0; 3; 6]

  (** Get all filled positions from board *)
  let get_filled_positions board =
    let positions = ref [] in
    for row = 0 to Constants.board_size - 1 do
      for col = 0 to Constants.board_size - 1 do
        match board.(row).(col) with
        | Fixed _ -> positions := (row, col) :: !positions
        | Empty | Mutable _ -> ()
      done
    done ;
    Array.of_list !positions

  (** Deep copy a board *)
  let copy_board board = Array.map Array.copy board
end

(** Fill a 3x3 diagonal box with shuffled numbers *)
let fill_diagonal_box board row col ~pool ~pool_index ~rng =
  if not (BoardUtils.is_valid_diagonal_pos row col)
  then Error (Invalid_diagonal_position (row, col))
  else
    let nums = ShufflePool.get_shuffled pool ~index:pool_index ~rng in
    let k = ref 0 in
    for i = 0 to Constants.box_size - 1 do
      for j = 0 to Constants.box_size - 1 do
        board.(row + i).(col + j) <- Fixed nums.(!k) ;
        incr k
      done
    done ;
    Ok ()

(** Solve board using backtracking with functional style *)
let solve_board board ~rng =
  let pool = ShufflePool.create ~rng () in
  let pool_index = ref 0 in

  let rec solve ~row ~col =
    match (row, col) with
    | 9, _ -> true (* Completed *)
    | _, 9 -> solve ~row:(row + 1) ~col:0 (* Next row *)
    | _ when board.(row).(col) <> Empty ->
        solve ~row ~col:(col + 1) (* Skip filled *)
    | _ ->
        let nums = ShufflePool.get_shuffled pool ~index:!pool_index ~rng in
        incr pool_index ;

        let rec try_numbers i =
          if i >= Constants.board_size
          then false
          else
            let num = nums.(i) in
            if Board_validation.is_valid_move board ~row ~col ~value:num
            then begin
              board.(row).(col) <- Fixed num ;
              if solve ~row ~col:(col + 1)
              then true
              else begin
                board.(row).(col) <- Empty ;
                try_numbers (i + 1)
              end
            end
            else try_numbers (i + 1)
        in
        try_numbers 0
  in
  if solve ~row:0 ~col:0 then Ok () else Error Board_solving_failed

(** Count solutions with early termination *)
let count_solutions board ~max_solutions =
  let board_copy = BoardUtils.copy_board board in
  let solution_count = ref 0 in

  let rec count_solve ~row ~col =
    if !solution_count >= max_solutions
    then ()
    else
      match (row, col) with
      | 9, _ -> incr solution_count (* Found solution *)
      | _, 9 -> count_solve ~row:(row + 1) ~col:0 (* Next row *)
      | _ when board_copy.(row).(col) <> Empty ->
          count_solve ~row ~col:(col + 1) (* Skip filled *)
      | _ ->
          for num = 1 to Constants.board_size do
            if
              !solution_count < max_solutions
              && Board_validation.is_valid_move board_copy ~row ~col ~value:num
            then begin
              board_copy.(row).(col) <- Fixed num ;
              count_solve ~row ~col:(col + 1) ;
              board_copy.(row).(col) <- Empty
            end
          done
  in
  count_solve ~row:0 ~col:0 ;
  !solution_count

(** Check if puzzle has exactly one solution *)
let has_unique_solution board = count_solutions board ~max_solutions:2 = 1

(** Remove cells strategically while maintaining unique solution *)
let remove_cells_strategically board ~cells_to_remove ~rng =
  let positions = BoardUtils.get_filled_positions board in
  let shuffled_positions = Fisher_yates.shuffle ~rng positions in
  let removed_count = ref 0 in
  let i = ref 0 in

  while
    !removed_count < cells_to_remove && !i < Array.length shuffled_positions
  do
    let row, col = shuffled_positions.(!i) in
    (match board.(row).(col) with
    | Fixed value ->
        board.(row).(col) <- Empty ;
        if has_unique_solution board
        then incr removed_count
        else board.(row).(col) <- Fixed value
    | Empty | Mutable _ -> () (* Skip non-fixed cells *)) ;
    incr i
  done ;
  !removed_count

(** Generate a complete Sudoku puzzle *)
let generate_random_board ?(difficulty = Easy)
    ?(rng = Random.State.make_self_init ()) () =
  let board = Board.create () in
  let pool = ShufflePool.create ~rng () in

  (* Phase 1: Fill diagonal boxes *)
  let fill_diagonals () =
    let rec fill_boxes positions pool_index =
      match positions with
      | [] -> Ok pool_index
      | (row, col) :: rest -> (
          match fill_diagonal_box board row col ~pool ~pool_index ~rng with
          | Ok () -> fill_boxes rest (pool_index + 1)
          | Error e -> Error e)
    in
    fill_boxes Constants.diagonal_positions 0
  in

  (* Phase 2: Complete the board *)
  let complete_board () = solve_board board ~rng in

  (* Phase 3: Remove cells strategically *)
  let create_puzzle () =
    let target_removals = difficulty_config difficulty in
    let actual_removed =
      remove_cells_strategically board ~cells_to_remove:target_removals ~rng
    in
    let remaining_cells = Constants.total_cells - actual_removed in

    Printf.printf
      "Generated %s puzzle: %d/%d cells removed, %d remaining (%.1f%% filled)\n"
      (match difficulty with
      | Easy -> "Easy"
      | Medium -> "Medium"
      | Hard -> "Hard")
      actual_removed target_removals remaining_cells
      (float_of_int remaining_cells
      /. float_of_int Constants.total_cells
      *. 100.0) ;

    Ok actual_removed
  in

  (* Execute all phases *)
  match fill_diagonals () with
  | Error e -> Error e
  | Ok _ -> (
      match complete_board () with
      | Error e -> Error e
      | Ok () -> (
          match create_puzzle () with
          | Error e -> Error e
          | Ok _ -> Ok (Board.to_array board)))

(** Generate board with error handling *)
let generate_safe ?(difficulty = Easy) ?(rng = Random.State.make_self_init ())
    () =
  match generate_random_board ~difficulty ~rng () with
  | Ok board -> Some board
  | Error err ->
      Printf.eprintf "Board generation failed: %s\n"
        (match err with
        | Invalid_diagonal_position (r, c) ->
            Printf.sprintf "Invalid diagonal position (%d,%d)" r c
        | Board_solving_failed -> "Failed to solve board"
        | Invalid_board_state -> "Invalid board state") ;
      None
