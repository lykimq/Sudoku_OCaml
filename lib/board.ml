(** Cell representation using variant types for type safety and clarity.

    Trade-offs: Memory overhead (1 word + tag vs raw int) for type safety and
    semantic clarity. Design: Prevents accidental modification of Fixed cells
    through type system. Pattern matching: Exhaustive compiler-checked cases. *)
type cell =
  | Empty
  | Fixed of int (* Initial puzzle clues, immutable *)
  | Mutable of int (* Player moves, can be modified *)

(** Board representation using 2D array for O(1) random access.

    Choice: Array over alternatives for performance.
    - vs Lists: O(n) access but more functional
    - vs Single array: Less readable, error-prone indexing
    - vs Immutable Map: Better for undo/redo but slower updates

    Memory: O(81) = O(1) for fixed-size Sudoku board. *)
type t = cell array array

let create () = Array.make_matrix 9 9 Empty

let of_array array =
  Array.init 9 (fun i ->
      Array.init 9 (fun j ->
          match array.(i).(j) with
          | 0 -> Empty
          | n when n >= 1 && n <= 9 -> Fixed n
          | _ -> failwith "Invalid cell value"))

(** Converts board to integer array, losing type information.

    Trade-off: Loses Fixed vs Mutable distinction for simple representation. *)
let to_array board =
  Array.map (Array.map (function Empty -> 0 | Fixed n | Mutable n -> n)) board

(** Pretty printer for debugging and display. *)
let pp fmt board =
  for i = 0 to 8 do
    if i mod 3 = 0 && i > 0 then Format.fprintf fmt "-------------------@," ;
    for j = 0 to 8 do
      if j mod 3 = 0 && j > 0 then Format.fprintf fmt "|" ;
      match board.(i).(j) with
      | Empty -> Format.fprintf fmt " ."
      | Fixed n | Mutable n -> Format.fprintf fmt " %d" n
    done ;
    Format.fprintf fmt "@,"
  done ;
  Format.fprintf fmt "@]"
