open Board

(* - Constants: Configuration management

   - Fisher-Yates: Randomization utilities

   - ShufflePool: Performance optimization

   - BoardUtils: Common operations and validations

   - Main Generation: Pipeline orchestration *)

module Constants = struct
  let board_size = 9

  let box_size = 3

  let total_cells = 81

  let pool_size = 10

  let diagonal_positions = [(0, 0); (3, 3); (6, 6)]
end

(* Puzzle difficulty levels with associated cell removal targets. *)
type difficulty = Easy | Medium | Hard

(* Configuration for difficulty levels. *)
let difficulty_config = function
  | Easy ->
      35 (* ~57% filled *)
  | Medium ->
      45 (* ~44% filled *)
  | Hard ->
      55 (* ~32% filled *)

(* Error types for better error handling and debugging. *)
type generation_error =
  | Invalid_diagonal_position of int * int
  | Board_solving_failed
  | Invalid_board_state

(* Instead of exceptions, use a Result type to handle errors explicitly. *)
type 'a result = ('a, generation_error) Result.t

(* Fisher-Yates shuffle:

   - Mathematically proven unbiased distribution

   - O(n) time, O(1) space complexity vs O(n log n) for sort-based shuffle

   - Separate RNG state for reproducible testing *)
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
    (* Deep copy the array to avoid modifying the original array. *)
    let arr_copy = Array.copy arr in
    shuffle_inplace ~rng arr_copy ;
    arr_copy
end

(* Immutable shuffle pool to avoid global mutable state and improve performance.
   Pre-computed shuffled arrays for efficient repeated randomization during
   board generation process. *)
module ShufflePool = struct
  type t = int array array

  (* Create a new shuffle pool with pre-shuffled arrays *)
  let create ?(rng = Random.State.make_self_init ()) () : t =
    let pool = Array.make Constants.pool_size [||] in
    for i = 0 to Constants.pool_size - 1 do
      let nums = Array.init Constants.board_size (fun j -> j + 1) in
      Fisher_yates.shuffle_inplace ~rng nums ;
      pool.(i) <- nums
    done ;
    pool

  (* Get a shuffled array from the pool, cycling through indices *)
  let get_shuffled pool ~index ~rng =
    let pool_idx = index mod Constants.pool_size in
    let nums = Array.copy pool.(pool_idx) in
    (* Extra shuffle to ensure randomness *)
    Fisher_yates.shuffle_inplace ~rng nums ;
    nums
end

(* Board utilities for common operations and validations. *)
module BoardUtils = struct
  (* Validate diagonal box position *)
  let is_valid_diagonal_pos row col =
    List.mem row [0; 3; 6] && List.mem col [0; 3; 6]

  (* Get all filled positions from board *)
  let get_filled_positions board =
    let positions = ref [] in
    for row = 0 to Constants.board_size - 1 do
      for col = 0 to Constants.board_size - 1 do
        match board.(row).(col) with
        | Fixed _ ->
            positions := (row, col) :: !positions
        | Empty | Mutable _ ->
            ()
      done
    done ;
    Array.of_list !positions

  (** Deep copy a board *)
  let copy_board board = Array.map Array.copy board
end

(* Fill a (3x3) diagonal box with shuffled numbers using constraint-free
   approach. Diagonal boxes don't share rows, columns, or boxes with each other.
   Enables parallel filling without constraint checking. Reduces backtracking
   complexity in subsequent solving phase. Mathematical optimization based on
   Sudoku constraint structure. *)
let fill_diagonal_box board row col ~pool ~pool_index ~rng =
  if not (BoardUtils.is_valid_diagonal_pos row col) then
    Error (Invalid_diagonal_position (row, col))
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

(* Solve board using backtracking with functional style and randomization.
   Randomized value ordering prevents systematic bias.

   Alternative: Dancing Links, constraint propagation, or SAT solvers would be
   more efficient but less educational *)
let solve_board board ~rng =
  let pool = ShufflePool.create ~rng () in
  let pool_index = ref 0 in
  let rec solve ~row ~col =
    match (row, col) with
    | 9, _ ->
        true (* Completed *)
    | _, 9 ->
        solve ~row:(row + 1) ~col:0 (* Next row *)
    | _ when board.(row).(col) <> Empty ->
        solve ~row ~col:(col + 1) (* Skip filled *)
    | _ ->
        (* Get randomized numbers for current cell*)
        let nums = ShufflePool.get_shuffled pool ~index:!pool_index ~rng in
        incr pool_index ;
        (* Try each number in random order. *)
        let rec try_numbers i =
          if i >= Constants.board_size then false
          else
            let num = nums.(i) in
            (* Check if the number is valid for the current cell. *)
            if Board_validation.is_valid_move board ~row ~col ~value:num then (
              (* Place the number in the current cell. *)
              board.(row).(col) <- Fixed num ;
              (* Try next cell. *)
              if solve ~row ~col:(col + 1) then true
              else (
                (* Backtrack. *)
                board.(row).(col) <- Empty ;
                try_numbers (i + 1) ) )
            (* Try next number. *)
              else try_numbers (i + 1)
        in
        (* Try first number. *)
        try_numbers 0
  in
  if solve ~row:0 ~col:0 then Ok () else Error Board_solving_failed

(* A valid Sudoku puzzle has only one solution. Count solutions with early
   termination helps to improve the performance massively instead of going
   through all possible solutions.

   - Time: O(9^k) worst case where k is the number of empty cells, often much
   better with early termination

   - Space: O(n²) for board copy + O(k) for recursion

   Alternative: SAT solver or constraint propagation would be faster but adds
   complexity and dependencies *)
let count_solutions board ~max_solutions =
  (* Create a copy of the board to avoid mutating the original board. *)
  let board_copy = BoardUtils.copy_board board in
  let solution_count = ref 0 in
  let rec count_solve ~row ~col =
    if !solution_count >= max_solutions then ()
    else
      match (row, col) with
      | 9, _ ->
          incr solution_count (* Found solution *)
      | _, 9 ->
          count_solve ~row:(row + 1) ~col:0 (* Next row *)
      | _ when board_copy.(row).(col) <> Empty ->
          count_solve ~row ~col:(col + 1) (* Skip filled *)
      | _ ->
          for num = 1 to Constants.board_size do
            (* Check if the number is valid for the current cell. *)
            if
              !solution_count < max_solutions
              && Board_validation.is_valid_move board_copy ~row ~col ~value:num
            then (
              (* Place the number in the current cell. *)
              board_copy.(row).(col) <- Fixed num ;
              (* Try next cell. *)
              count_solve ~row ~col:(col + 1) ;
              (* Backtrack. *)
              board_copy.(row).(col) <- Empty )
          done
  in
  count_solve ~row:0 ~col:0 ; !solution_count

(* Check if puzzle has exactly one solution. *)
let has_unique_solution board = count_solutions board ~max_solutions:2 = 1

(* Remove cells strategically while maintaining unique solution.

   Steps:

   - Get all filled positions and shuffle randomly.

   - For each position attempt removal:

   + Temporarily remove cell value

   + Check if puzzle still has unique solution

   + Keep removal if valid, restore if invalid

   - Continue until target removals reached or positions exhausted

   - Time: O(target × solution_checking_time) = potentially expensive

   - Space: O(n²) for position shuffling

   Alternative: Could use constraint propagation or more sophisticated
   algorithms like "minimal puzzle" generation *)
let remove_cells_strategically board ~cells_to_remove ~rng =
  (* Get all filled positions and shuffle randomly. *)
  let positions = BoardUtils.get_filled_positions board in
  (* Shuffle positions randomly. *)
  let shuffled_positions = Fisher_yates.shuffle ~rng positions in
  (* Remove cells strategically. *)
  let removed_count = ref 0 in
  let i = ref 0 in
  (* Remove cells strategically. *)
  while
    !removed_count < cells_to_remove && !i < Array.length shuffled_positions
  do
    (* Get the next position to remove. *)
    let row, col = shuffled_positions.(!i) in
    (* Temporarily remove the cell value. *)
    ( match board.(row).(col) with
    | Fixed value ->
        (* Temporarily remove the cell value. *)
        board.(row).(col) <- Empty ;
        (* Check if puzzle still has unique solution. *)
        if has_unique_solution board then
          (* Increment the removed count. *)
          incr removed_count
        else
          (* Restore the cell value. *)
          board.(row).(col) <- Fixed value
    | Empty | Mutable _ ->
        () (* Skip non-fixed cells *) ) ;
    (* Increment the position index. *)
    incr i
  done ;
  !removed_count

(* Generate a complete Sudoku puzzle with specified difficulty.

   - Multi-phase pipeline with Result type error handling

   - Phase 1: Fill diagonal boxes (constraint-free foundation)

   - Phase 2: Complete board via backtracking (constraint satisfaction)

   - Phase 3: Remove cells strategically (puzzle creation)

   - Time: O(1) for diagonal filling + O(9^k) backtracking + O(target ×
   solution_checking) strategic removal

   - Space: O(n²) for board copy + O(k) for recursion

   - Error handling: Result type with specific error contexts

   - Invalid diagonal positions (programming error)

   - Board solving failures (rare but possible)

   - Invalid board states (should not occur with proper implementation)

   Alternative: Could separate into distinct functions for each phase or use
   different generation algorithms entirely *)
let generate_random_board ?(difficulty = Easy)
    ?(rng = Random.State.make_self_init ()) () =
  let board = Board.create () in
  let pool = ShufflePool.create ~rng () in
  (* Phase 1: Fill diagonal boxes *)
  let fill_diagonals () =
    let rec fill_boxes positions pool_index =
      match positions with
      | [] ->
          Ok pool_index
      | (row, col) :: rest -> (
        match fill_diagonal_box board row col ~pool ~pool_index ~rng with
        | Ok () ->
            fill_boxes rest (pool_index + 1)
        | Error e ->
            Error e )
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
      ( match difficulty with
      | Easy ->
          "Easy"
      | Medium ->
          "Medium"
      | Hard ->
          "Hard" )
      actual_removed target_removals remaining_cells
      ( float_of_int remaining_cells
      /. float_of_int Constants.total_cells
      *. 100.0 ) ;
    Ok actual_removed
  in
  (* Execute all phases *)
  match fill_diagonals () with
  | Error e ->
      Error e
  | Ok _ -> (
    match complete_board () with
    | Error e ->
        Error e
    | Ok () -> (
      match create_puzzle () with
      | Error e ->
          Error e
      | Ok _ ->
          Ok (Board.to_array board) ) )

(* Generate board with error handling and Option type interface instead of
   Result type for simpler client code.

   Alternative: Could expose Result type to caller for more precise error
   handling, or use exception-based approach *)
let generate_safe ?(difficulty = Easy) ?(rng = Random.State.make_self_init ())
    () =
  match generate_random_board ~difficulty ~rng () with
  | Ok board ->
      Some board
  | Error err ->
      Printf.eprintf "Board generation failed: %s\n"
        ( match err with
        | Invalid_diagonal_position (r, c) ->
            Printf.sprintf "Invalid diagonal position (%d,%d)" r c
        | Board_solving_failed ->
            "Failed to solve board"
        | Invalid_board_state ->
            "Invalid board state" ) ;
      None
