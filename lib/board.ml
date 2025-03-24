type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

let create () = Array.make_matrix 9 9 Empty

let of_array array =
  Array.init 9 (fun i ->
      Array.init 9 (fun j ->
          match array.(i).(j) with
          | 0 -> Empty
          | n when n >= 1 && n <= 9 -> Fixed n
          | _ -> failwith "Invalid cell value"))

let to_array board =
  Array.map (Array.map (function Empty -> 0 | Fixed n | Mutable n -> n)) board

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
