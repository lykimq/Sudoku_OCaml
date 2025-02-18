type difficulty =
  | Easy
  | Medium
  | Hard

let generate_random_board ?(difficulty = Easy) () =
  Random.self_init () ;
  let board = Array.make_matrix 9 9 0 in
  let _rng = Random.State.make_self_init () in

  (* Fill diagonal 3x3 boxes *)
  let fill_diagonal_box start_row start_col =
    let nums = Array.init 9 (fun i -> i + 1) in
    for i = 8 downto 1 do
      let j = Random.int (i + 1) in
      let temp = nums.(i) in
      nums.(i) <- nums.(j) ;
      nums.(j) <- temp
    done ;
    let k = ref 0 in
    for i = 0 to 2 do
      for j = 0 to 2 do
        board.(start_row + i).(start_col + j) <- nums.(!k) ;
        incr k
      done
    done
  in

  (* Fill the diagonal 3x3 boxes *)
  fill_diagonal_box 0 0 ;
  fill_diagonal_box 3 3 ;
  fill_diagonal_box 6 6 ;

  (* Use backtracking to fill the rest of the board *)
  let is_valid row col num =
    let block_row = row / 3 * 3 in
    let block_col = col / 3 * 3 in
    let valid = ref true in

    (* Check row and column *)
    for i = 0 to 8 do
      if board.(row).(i) = num || board.(i).(col) = num then valid := false
    done ;

    (* Check 3x3 box *)
    for i = 0 to 2 do
      for j = 0 to 2 do
        if board.(block_row + i).(block_col + j) = num then valid := false
      done
    done ;
    !valid
  in
  let rec solve row col =
    if row = 9
    then true
    else if col = 9
    then solve (row + 1) 0
    else if board.(row).(col) <> 0
    then solve row (col + 1)
    else
      let nums = Array.init 9 (fun i -> i + 1) in
      for i = 8 downto 1 do
        let j = Random.int (i + 1) in
        let temp = nums.(i) in
        nums.(i) <- nums.(j) ;
        nums.(j) <- temp
      done ;
      let found = ref false in
      let i = ref 0 in
      while (not !found) && !i < 9 do
        let num = nums.(!i) in
        if is_valid row col num
        then (
          board.(row).(col) <- num ;
          if solve row (col + 1) then found := true else board.(row).(col) <- 0) ;
        incr i
      done ;
      !found
  in
  ignore (solve 0 0) ;

  (* Remove some numbers to create a puzzle *)
  let cells_to_keep =
    match difficulty with
    | Easy -> 35 (* keep ~35% cells filled *)
    | Medium -> 30 (* keep ~30% cells filled *)
    | Hard -> 25 (* keep ~25% cells filled *)
  in
  let total_cells = 81 in
  let cells_to_remove = total_cells - cells_to_keep in
  let removed = ref 0 in
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
