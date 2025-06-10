open Board

(* Core mutation function using higher-order functions for flexible cell
   updates.

   Alternative approaches considered: - Copy-on-write: Complex implementation,
   marginal benefit for small boards - Zipper data structure: Good for
   undo/redo, but complex API - Persistent data structures: Excellent for
   functional programming, library dependency *)
let update_cell board ~row ~col ~f =
  (* Fail fast on invalid positions *)
  if not (Board_validation.in_bounds row && Board_validation.in_bounds col) then
    None
  else
    match board.(row).(col) with
    (* Fixed cells cannot be modified - protects puzzle integrity *)
    | Fixed _ ->
        None
    | Empty | Mutable _ ->
        (* Immutable update: deep copy entire board *)
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- f board.(row).(col) ;
        Some new_board

(* Clears a cell by setting it to Empty. *)
let clear_cell board ~row ~col = update_cell board ~row ~col ~f:(fun _ -> Empty)

(* Sets a cell value with two-phase validation.

   Phase 1: Structural validation
   (bounds checking, cell mutability)

   - Delegates to update_cell for consistency

   - Fails fast on Fixed cells or invalid coordinates

   Phase 2: Game logic validation (Sudoku constraint checking)*)
let set_cell board ~row ~col ~value =
  match update_cell board ~row ~col ~f:(fun _ -> Mutable value) with
  | None ->
      None
  | Some new_board ->
      (* Validate against original board state to check if move would be
         legal *)
      if Board_validation.is_valid_move board ~row ~col ~value then (
        (* Side effect: Clear invalid marking (impure but pragmatic for UI
           feedback) *)
        Invalid_cells.clear_invalid ~row ~col ;
        Some (new_board, true) )
      else (
        (* Side effect: Mark invalid for user feedback - prevents silent
           failures *)
        Invalid_cells.mark_invalid ~row ~col ;
        Some (new_board, false) )
