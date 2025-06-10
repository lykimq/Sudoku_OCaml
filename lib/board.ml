(* Cell representation using variant types for type safety and clarity.

   Design Choice: Variant types over alternatives for several reasons:

   - vs Raw integers: less type-safe, less flexible, no pattern matching, cannot
   distinguish between fixed and mutable cells

   - vs Records: type-safe, more flexible, but more verbose, limited pattern
   matching

   - vs Objects: Functional paradigm alignment, more memory overhead, limited
   pattern matching, slow in performance, no pattern matching

   Algorithm: Direct memory representation with tag+value encoding - Empty: Just
   tag (1 word) - Fixed/Mutable n: Tag + int value (2 words)*)
type cell =
  | Empty
  | Fixed of int (* Initial puzzle clues, immutable *)
  | Mutable of int (* Player moves, can be modified *)

(* Board representation using 2D array for O(1) random access.

   Design Choice: Array over alternatives:

   - vs List of lists: O(n) access, but more functionally pure

   - vs Single array with index calculation: Less readable, error-prone

   - vs Immutable Map with (row,col) keys: Better for undo/redo, slower O(log n)

   - vs Mutable records: Complex update patterns, less composable

   Algorithm: Fixed-size 9x9 matrix for constant-time access Memory: O(81) =
   O(1) for Sudoku, predictable cache behavior *)
type t = cell array array

(* Creates empty 9x9 Sudoku board.*)
let create () = Array.make_matrix 9 9 Empty

(* Serialization/Deserialization functions, the serialization is a lossy
   conversion, but the deserialization is a pure function, both takes O(n^2)
   time, and space for new array *)
let of_array array =
  Array.init 9 (fun i ->
      Array.init 9 (fun j ->
          match array.(i).(j) with
          | 0 -> Empty
          | n when n >= 1 && n <= 9 -> Fixed n
          | _ -> failwith "Invalid cell value"))

let to_array board =
  Array.map (Array.map (function Empty -> 0 | Fixed n | Mutable n -> n)) board
