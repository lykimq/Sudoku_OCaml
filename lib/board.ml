type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

let create () = Array.make_matrix 9 9 Empty

(* Convert array to board *)
let of_array array =
  let board = create () in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j value ->
          board.(i).(j) <- (if value = 0 then Empty else Fixed value))
        row)
    array ;
  board

(* Convert board to array *)
let to_array board =
  Array.map (Array.map (function Empty -> 0 | Fixed n | Mutable n -> n)) board

(* Print board *)
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

