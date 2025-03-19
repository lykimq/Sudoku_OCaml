(* Represents the difficulty level of a Sudoku puzzle, which determines how many
   cells will be filled in the final puzzle. *)
type difficulty =
  | Easy (* More cells revealed, easier to solve *)
  | Medium (* Moderate number of cells revealed *)
  | Hard (* Fewer cells revealed, more challenging *)

(* Fills a 3x3 diagonal box with random numbers 1-9.

   This function uses the Fisher-Yates shuffle algorithm to randomize the
   numbers, then places them in the 3x3 box starting at the given position.

   board: The game board array to modify start_row: The starting row of the 3x3
   box (should be 0, 3, or 6) start_col: The starting column of the 3x3 box
   (should be 0, 3, or 6) *)
let fill_diagonal_box board start_row start_col =
  if not (List.mem start_row [0; 3; 6] && List.mem start_col [0; 3; 6])
  then failwith "Invalid start position: Must be (0,3,6) for diagonal boxes" ;

  let nums = Array.init 9 (fun i -> i + 1) in
  (* Fisher-Yates shuffle algorithm *)
  for i = 8 downto 1 do
    let j = Random.int (i + 1) in
    let temp = nums.(i) in
    nums.(i) <- nums.(j) ;
    nums.(j) <- temp
  done ;
  (* Fill the 3x3 box *)
  let k = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      board.(start_row + i).(start_col + j) <- nums.(!k) ;
      incr k
    done
  done

(* Generates a new random Sudoku board with the specified difficulty level.

   The generation process works as follows: 1. Fill the three diagonal 3x3 boxes
   with random numbers 2. Use backtracking to fill the rest of the board with a
   valid solution 3. Remove a certain number of cells based on the difficulty
   level *)
let generate_random_board ?(difficulty = Easy) () =
  Random.self_init () ;
  let board = Array.make_matrix 9 9 0 in
  let _rng = Random.State.make_self_init () in

  (* Fill diagonal 3x3 boxes *)
  let fill_diagonal_box start_row start_col =
    fill_diagonal_box board start_row start_col
  in

  (* Fill the diagonal 3x3 boxes: (0,0) (0,3) (0,6) (3,0) (3,3) (3,6) (6,0)
     (6,3) (6,6) *)
  fill_diagonal_box 0 0 ;
  (* Top left box *)
  fill_diagonal_box 3 3 ;
  (* Center Top-left box *)
  fill_diagonal_box 6 6 ;

  (* Bottom Top-left box *)

  (* Recursive backtracking algorithm that fills the rest of the board.

     This function: - Starts at position (0, 0) and proceeds row by row, column
     by column - Tries to fill each empty cell with a valid random number (1-9)
     - If it cannot find a valid number, backtracks and tries different numbers
     for previous cells - Continues until the entire board is filled with valid
     numbers *)
  let rec solve row col =
    if row = 9
    then true (* Reached end of board - solution found *)
    else if col = 9
    then solve (row + 1) 0 (* Move to next row *)
    else if board.(row).(col) <> 0
    then solve row (col + 1) (* Skip filled cells *)
    else
      (* Try random numbers at this position *)
      let nums = Array.init 9 (fun i -> i + 1) in
      (* Shuffle the numbers for randomness *)
      for i = 8 downto 1 do
        let j = Random.int (i + 1) in
        let temp = nums.(i) in
        nums.(i) <- nums.(j) ;
        nums.(j) <- temp
      done ;
      let found = ref false in
      let i = ref 0 in
      (* Try each number until a valid solution is found *)
      while (not !found) && !i < 9 do
        let num = nums.(!i) in
        if Validation_board.is_valid_number (Board.of_array board) row col num
        then (
          board.(row).(col) <- num ;
          if solve row (col + 1) then found := true else board.(row).(col) <- 0) ;
        incr i
      done ;
      !found
  in
  (* Start the solving process from the top-left corner *)
  ignore (solve 0 0) ;

  (* Remove some numbers to create a puzzle based on difficulty *)
  let cells_to_keep =
    match difficulty with
    | Easy -> 35 (* keep ~35% cells filled *)
    | Medium -> 30 (* keep ~30% cells filled *)
    | Hard -> 25 (* keep ~25% cells filled *)
  in
  let total_cells = 81 in
  let cells_to_remove = total_cells - cells_to_keep in
  let removed = ref 0 in
  (* Randomly remove cells until we reach the target difficulty *)
  while !removed < cells_to_remove do
    let row = Random.int 9 in
    let col = Random.int 9 in
    if board.(row).(col) <> 0
    then (
      board.(row).(col) <- 0 ;
      incr removed)
  done ;

  board

let generate_easy_board () = generate_random_board ~difficulty:Easy ()
let generate_medium_board () = generate_random_board ~difficulty:Medium ()
let generate_hard_board () = generate_random_board ~difficulty:Hard ()
