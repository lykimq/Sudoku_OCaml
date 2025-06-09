open Board

(** Difficulty levels determining puzzle complexity by number of filled cells.
*)
type difficulty =
  | Easy
  | Medium
  | Hard

(** Fisher-Yates shuffle with custom RNG support.

    Complexity: O(n) time, O(n) space (for array copy). Design: Immutable
    approach - copies array to preserve original. Security: Uses OCaml's
    Random.State (not cryptographically secure). Testing: Custom RNG parameter
    enables deterministic testing. *)
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

    Strategy: Diagonal boxes are independent (no conflicts) - reduces search
    space. Algorithm: Shuffle numbers 1-9, then fill box sequentially. Input
    validation: Critical for preventing bounds errors. *)
let fill_diagonal_box board start_row start_col ~rng =
  (* Validate diagonal box positions (0,0), (3,3), (6,6) *)
  if not (List.mem start_row [0; 3; 6] && List.mem start_col [0; 3; 6])
  then failwith "Invalid start position: Must be (0,3,6) for diagonal boxes" ;

  let nums = Array.init 9 (fun i -> i + 1) in
  let nums = shuffle_array ~rng nums in
  (* Fill 3x3 box with shuffled numbers *)
  let k = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      board.(start_row + i).(start_col + j) <- Fixed nums.(!k) ;
      incr k
    done
  done

(** Backtracking algorithm to complete the board.

    Algorithm: Row-by-row, col-by-col with constraint checking. Why
    backtracking: Simple, guaranteed solution, provides variety. Alternatives
    considered: Constraint satisfaction (complex), Dancing Links (overkill).

    Process: Try random numbers 1-9, backtrack on conflicts, recurse until
    complete. *)
let rec solve board ~row ~col ~rng =
  if row = 9
  then true (* Reached end - solution found *)
  else if col = 9
  then solve board ~row:(row + 1) ~col:0 ~rng (* Next row *)
  else if board.(row).(col) <> Empty
  then solve board ~row ~col:(col + 1) ~rng (* Skip filled cells *)
  else
    (* Try random numbers for empty cells *)
    let nums = Array.init 9 (fun i -> i + 1) in
    let nums = shuffle_array ~rng nums in
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i < 9 do
      let num = nums.(!i) in
      if Board_validation.is_valid_move board ~row ~col ~value:num
      then (
        board.(row).(col) <- Fixed num ;
        (* Tentative assignment *)
        if solve board ~row ~col:(col + 1) ~rng
        then found := true
        else board.(row).(col) <- Empty) ;
      (* Backtrack *)
      incr i
    done ;
    !found

(** Generates random Sudoku puzzle with specified difficulty.

    Algorithm: 1. Fill diagonal boxes (reduces search space) 2. Complete board
    with backtracking 3. Remove cells based on difficulty

    Complexity: Dominated by backtracking solve phase O(9^m).

    Quality trade-offs:
    - Random removal may not guarantee unique solutions
    - No symmetry or aesthetic constraints
    - Could be improved with solution uniqueness verification *)
let generate_random_board ?(difficulty = Easy) () =
  let board = Board.create () in
  let rng = Random.State.make_self_init () in

  (* Phase 1: Fill diagonal boxes to reduce search space *)
  List.iter
    (fun (r, c) -> fill_diagonal_box board r c ~rng)
    [(0, 0); (3, 3); (6, 6)] ;

  (* Phase 2: Complete board using backtracking *)
  ignore (solve board ~row:0 ~col:0 ~rng) ;

  (* Phase 3: Create puzzle by removing cells *)
  let total_cells = 81 in
  let cells_to_keep =
    match difficulty with
    | Easy -> 70 (* ~55% filled - beginner friendly *)
    | Medium -> 30 (* ~30% filled - moderate challenge *)
    | Hard -> 25 (* ~25% filled - significant challenge *)
  in
  let cells_to_remove = total_cells - cells_to_keep in
  let removed = ref 0 in
  (* Random removal - could be improved for unique solutions *)
  while !removed < cells_to_remove do
    let row = Random.State.int rng 9 in
    let col = Random.State.int rng 9 in
    match board.(row).(col) with
    | Fixed _ ->
        board.(row).(col) <- Empty ;
        incr removed
    | Empty | Mutable _ -> ()
  done ;
  Board.to_array board
