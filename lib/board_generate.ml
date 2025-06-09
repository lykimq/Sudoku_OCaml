open Board

(** Puzzle difficulty levels that control how many cells are removed from the
    complete board. *)
type difficulty =
  | Easy
  | Medium
  | Hard

(** Shuffles an array randomly using the Fisher-Yates algorithm.

    Takes an optional random number generator for reproducible testing. Returns
    a new shuffled array without modifying the original. *)
let shuffle_array ?(rng = Random.State.make_self_init ()) arr =
  let array = Array.copy arr in
  let len = Array.length array in
  for i = len - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let temp = array.(i) in
    array.(i) <- array.(j) ;
    array.(j) <- temp
  done ;
  array

(** Fills a 3x3 diagonal box with random numbers 1-9.

    Diagonal boxes at positions (0,0), (3,3), and (6,6) don't share rows,
    columns, or overlap with each other, so they can be filled independently.
    This reduces the complexity of completing the board later. *)
let fill_diagonal_box board start_row start_col ~rng =
  (* Only allow diagonal box positions *)
  if not (List.mem start_row [0; 3; 6] && List.mem start_col [0; 3; 6])
  then failwith "Invalid start position: Must be (0,3,6) for diagonal boxes" ;

  let nums = Array.init 9 (fun i -> i + 1) in
  let nums = shuffle_array ~rng nums in
  (* Fill the 3x3 box with shuffled numbers *)
  let k = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      board.(start_row + i).(start_col + j) <- Fixed nums.(!k) ;
      incr k
    done
  done

(** Completes the board using backtracking after diagonal boxes are filled.

    Traverses the board row by row, column by column. For each empty cell, tries
    numbers 1-9 in random order until finding one that satisfies Sudoku
    constraints. If no number works, backtracks and tries a different number in
    the previous cell. *)
let rec solve board ~row ~col ~rng =
  if row = 9
  then true (* Reached the end - board is complete *)
  else if col = 9
  then solve board ~row:(row + 1) ~col:0 ~rng (* Move to next row *)
  else if board.(row).(col) <> Empty
  then solve board ~row ~col:(col + 1) ~rng (* Skip already filled cells *)
  else
    (* Try numbers 1-9 in random order for empty cells *)
    let nums = Array.init 9 (fun i -> i + 1) in
    let nums = shuffle_array ~rng nums in
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i < 9 do
      let num = nums.(!i) in
      if Board_validation.is_valid_move board ~row ~col ~value:num
      then (
        board.(row).(col) <- Fixed num ;
        (* Try to solve the rest of the board *)
        if solve board ~row ~col:(col + 1) ~rng
        then found := true
        else board.(row).(col) <- Empty) ;
      (* Backtrack if this number doesn't work *)
      incr i
    done ;
    !found

(** Counts how many different solutions a puzzle has, up to a maximum limit.

    Uses backtracking to systematically try all possible ways to fill the
    puzzle. Stops counting when it reaches the maximum to avoid unnecessary
    computation. For puzzle validation, we only need to know if there's exactly
    1 solution or more than 1. *)
let count_solutions board ~max_solutions =
  let board_copy = Array.map Array.copy board in
  let solution_count = ref 0 in

  let rec count_solve ~row ~col =
    if !solution_count >= max_solutions
    then () (* Stop if we've found enough solutions *)
    else if row = 9
    then incr solution_count (* Found a complete solution *)
    else if col = 9
    then count_solve ~row:(row + 1) ~col:0 (* Move to next row *)
    else if board_copy.(row).(col) <> Empty
    then count_solve ~row ~col:(col + 1) (* Skip filled cells *)
    else
      (* Try each number 1-9 in this empty cell *)
      for num = 1 to 9 do
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

(** Checks if a puzzle has exactly one solution.

    A valid Sudoku puzzle must have exactly one unique solution. Uses
    count_solutions with a limit of 2 for efficiency. *)
let has_unique_solution board = count_solutions board ~max_solutions:2 = 1

(** Gets all positions with Fixed cells in random order.

    Returns an array of (row, col) coordinates for all cells that contain Fixed
    values, shuffled randomly. This is used to determine which cells to try
    removing when creating the puzzle. *)
let get_shuffled_filled_positions board ~rng =
  let positions = ref [] in
  for row = 0 to 8 do
    for col = 0 to 8 do
      match board.(row).(col) with
      | Fixed _ -> positions := (row, col) :: !positions
      | Empty | Mutable _ -> ()
    done
  done ;
  let pos_array = Array.of_list !positions in
  shuffle_array ~rng pos_array

(** Removes cells from the board while ensuring the puzzle keeps exactly one
    solution.

    Algorithm:

    1. Get all filled cell positions in random order

    2. For each position, temporarily remove the cell

    3. Check if the puzzle still has exactly one solution

    4. If yes, keep the removal; if no, restore the cell

    5. Continue until we've removed enough cells or no more can be safely
    removed

    This guarantees that the final puzzle has exactly one unique solution. *)
let remove_cells_strategically board ~cells_to_remove ~rng =
  let positions = get_shuffled_filled_positions board ~rng in
  let removed_count = ref 0 in
  let i = ref 0 in

  while !removed_count < cells_to_remove && !i < Array.length positions do
    let row, col = positions.(!i) in
    (match board.(row).(col) with
    | Fixed value ->
        (* Temporarily remove this cell *)
        board.(row).(col) <- Empty ;

        (* Check if puzzle still has exactly one solution *)
        if has_unique_solution board
        then begin
          (* Safe to remove - keep it empty *)
          incr removed_count
        end
        else begin
          (* Would create multiple solutions - restore the cell *)
          board.(row).(col) <- Fixed value
        end
    | Empty | Mutable _ -> () (* Skip already empty cells *)) ;
    incr i
  done ;
  !removed_count

(** Generates a Sudoku puzzle with the specified difficulty.

    Three-phase process:

    1. Fill diagonal 3×3 boxes with random numbers (reduces complexity)

    2. Complete the entire board using backtracking

    3. Remove cells strategically while maintaining exactly one solution

    The result is a valid Sudoku puzzle with exactly one unique solution.
    Difficulty is controlled by how many cells are removed:
    - Easy: ~35 cells removed (~57% filled)
    - Medium: ~45 cells removed (~44% filled)
    - Hard: ~55 cells removed (~32% filled)

    Note: Actual number of removed cells may be less than the target if removing
    more would create multiple solutions. *)
let generate_random_board ?(difficulty = Easy) () =
  let board = Board.create () in
  let rng = Random.State.make_self_init () in

  (* Phase 1: Fill diagonal boxes to simplify completion *)
  List.iter
    (fun (r, c) -> fill_diagonal_box board r c ~rng)
    [(0, 0); (3, 3); (6, 6)] ;

  (* Phase 2: Complete the board using backtracking *)
  ignore (solve board ~row:0 ~col:0 ~rng) ;

  (* Phase 3: Remove cells to create the puzzle *)
  let cells_to_remove =
    match difficulty with
    | Easy -> 35 (* Easier puzzle with more clues *)
    | Medium -> 45 (* Moderate difficulty *)
    | Hard -> 55 (* Harder puzzle with fewer clues *)
  in

  let actual_removed = remove_cells_strategically board ~cells_to_remove ~rng in

  (* Show how many cells were actually removed *)
  let remaining_cells = 81 - actual_removed in
  Printf.printf
    "Generated %s puzzle: %d cells removed, %d remaining (%.1f%% filled)\n"
    (match difficulty with
    | Easy -> "Easy"
    | Medium -> "Medium"
    | Hard -> "Hard")
    actual_removed remaining_cells
    (float_of_int remaining_cells /. 81.0 *. 100.0) ;

  Board.to_array board
