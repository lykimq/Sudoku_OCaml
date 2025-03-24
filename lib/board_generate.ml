(* Represents the difficulty level of a Sudoku puzzle, which determines how many
   cells will be filled in the final puzzle. *)
type difficulty =
  | Easy
  | Medium
  | Hard

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

(* Fills a 3x3 diagonal box with random numbers 1-9.

   This function uses the Fisher-Yates shuffle algorithm to randomize the
   numbers, then places them in the 3x3 box starting at the given position.

   - board: The game board array to modify

   - start_row: The starting row of the 3x3 box (should be 0, 3, or 6)

   - start_col: The starting column of the 3x3 box (should be 0, 3, or 6)

   - rng: The random number generator to use *)
let fill_diagonal_box board start_row start_col ~rng =
  if not (List.mem start_row [0; 3; 6] && List.mem start_col [0; 3; 6])
  then failwith "Invalid start position: Must be (0,3,6) for diagonal boxes" ;

  let nums = Array.init 9 (fun i -> i + 1) in
  let nums = shuffle_array ~rng nums in
  (* Fill the 3x3 box *)
  let k = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      board.(start_row + i).(start_col + j) <- nums.(!k) ;
      incr k
    done
  done

(* Recursive backtracking algorithm that fills the rest of the board.

   - Starts at position (0, 0) and proceeds row by row, column by column.

   - Tries to fill each empty cell with a valid random number (1-9).

   - If it cannot find a valid number, backtracks and tries different numbers
   for previous cells.

   - Continues until the entire board is filled with valid numbers. *)
let rec solve board ~row ~col ~rng =
  if row = 9
  then true (* Reached end of board - solution found *)
  else if col = 9
  then solve board ~row:(row + 1) ~col:0 ~rng (* Move to next row *)
  else if board.(row).(col) <> 0 (* Skip filled cells *)
  then solve board ~row ~col:(col + 1) ~rng
  else
    (* Try random numbers at this position *)
    let nums = Array.init 9 (fun i -> i + 1) in
    let nums = shuffle_array ~rng nums in
    (* Try each number until a valid solution is found *)
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i < 9 do
      (* Get the next number to try *)
      let num = nums.(!i) in
      if Board_rules.is_valid_number (Board.of_array board) row col num
      then (
        (* Place the number in the current cell *)
        board.(row).(col) <- num ;
        if
          (* Recursively solve the rest of the board *)
          solve board ~row ~col:(col + 1) ~rng
        then found := true
        else
          (* Backtrack if no solution found *)
          board.(row).(col) <- 0) ;
      (* Try the next number *)
      incr i
    done ;
    !found

(* Generates a new random Sudoku board with the specified difficulty level.

   The generation process works as follows:

   - Fill the three diagonal 3x3 boxes with random numbers.

   - Use backtracking to fill the rest of the board with a valid solution

   - Remove a certain number of cells based on the difficulty level *)
let generate_random_board ?(difficulty = Easy) () =
  let board = Array.make_matrix 9 9 0 in
  let rng = Random.State.make_self_init () in

  (* Fill diagonal 3x3 boxes *)
  List.iter
    (fun (r, c) -> fill_diagonal_box board r c ~rng)
    [(0, 0); (3, 3); (6, 6)] ;

  (* Start the solving process from the top-left corner *)
  ignore (solve board ~row:0 ~col:0 ~rng) ;

  (* Remove some numbers to create a puzzle based on difficulty *)
  let total_cells = 81 in
  let cells_to_keep =
    match difficulty with
    | Easy -> 55 (* keep ~55% cells filled *)
    | Medium -> 30 (* keep ~30% cells filled *)
    | Hard -> 25 (* keep ~25% cells filled *)
  in
  let cells_to_remove = total_cells - cells_to_keep in
  let removed = ref 0 in
  (* Randomly remove cells until we reach the target difficulty *)
  while !removed < cells_to_remove do
    let row = Random.State.int rng 9 in
    let col = Random.State.int rng 9 in
    if board.(row).(col) <> 0
    then (
      board.(row).(col) <- 0 ;
      incr removed)
  done ;
  board
