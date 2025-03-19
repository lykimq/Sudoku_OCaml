open Cairo

type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array
type cell_state = Invalid

(* Colors *)
let background_color = (1., 1., 1.) (* white *)
let grid_color = (0., 0., 0.) (* black *)
let fixed_color = (0., 0., 0.)
let mutable_color = (0., 0., 1.) (* blue *)
let selected_color = (0.8, 0.8, 0.1) (* yellow *)
let error_color = (1., 0.2, 0.2) (* red *)


(* GTK+ related constants *)
let cell_size = 60.0
let board_size = 9. *. cell_size
let margin = 20.0
let total_size = int_of_float (board_size +. (2.0 *. margin))

(* Using Hashtbl to store invalid cells instead of maintaining a full board *)
let invalid_cells = Hashtbl.create 81
let mark_invalid ~row ~col = Hashtbl.replace invalid_cells (row, col) Invalid
let clear_invalid ~row ~col = Hashtbl.remove invalid_cells (row, col)
let is_invalid ~row ~col = Hashtbl.mem invalid_cells (row, col)
let create () = Array.make_matrix 9 9 Empty

(* Draw background *)
let drawing_background ctxt color x y =
  let r, g, b = color in
  Cairo.set_source_rgb ctxt r g b ;
  Cairo.rectangle ctxt x y ~w:cell_size ~h:cell_size ;
  Cairo.fill ctxt

(* Draw text *)
let draw_text ctxt text x y color font_size =
  let r, g, b = color in
  Cairo.set_source_rgb ctxt r g b ;
  Cairo.select_font_face ctxt "Sans" ~weight:Cairo.Bold ;
  Cairo.set_font_size ctxt font_size ;

  let extents = Cairo.text_extents ctxt text in
  let text_x = x +. ((cell_size -. extents.width) /. 2.0) in
  let text_y = y +. ((cell_size +. extents.height) /. 2.0) in
  Cairo.move_to ctxt text_x text_y ;
  Cairo.show_text ctxt text

(* Draw grid *)
let draw_grid ctxt =
  let gr, gg, gb = grid_color in
  Cairo.set_source_rgb ctxt gr gg gb ;

  (* Draw inner grid lines *)
  for i = 0 to 9 do
    let line_width = if i mod 3 = 0 then 2.0 else 1.0 in
    Cairo.set_line_width ctxt line_width ;

    (* Vertical lines *)
    let x = margin +. (float_of_int i *. cell_size) in
    Cairo.move_to ctxt x margin ;
    Cairo.line_to ctxt x (margin +. board_size) ;
    Cairo.stroke ctxt ;

    (* Horizontal lines *)
    let y = margin +. (float_of_int i *. cell_size) in
    Cairo.move_to ctxt margin y ;
    Cairo.line_to ctxt (margin +. board_size) y ;
    Cairo.stroke ctxt
  done ;

  (* Draw outer border *)
  Cairo.set_line_width ctxt 3.0 ;
  Cairo.rectangle ctxt margin margin ~w:board_size ~h:board_size ;
  Cairo.stroke ctxt

(* Draw board *)
let draw_board (ctx : Cairo.context) board selected =
  (* Clear background *)
  let br, bg, bb = background_color in
  Cairo.set_source_rgb ctx br bg bb ;
  Cairo.paint ctx ;

  (* Draw cells *)
  for row = 0 to 8 do
    for col = 0 to 8 do
      let x = margin +. (float_of_int col *. cell_size) in
      let y = margin +. (float_of_int row *. cell_size) in

      (* Draw cell background *)
      let bg_color =
        match selected with
        | Some (sr, sc) when sr = row && sc = col -> selected_color
        | _ -> background_color
      in
      drawing_background ctx bg_color x y ;

      (* Draw cell content *)
      match board.(row).(col) with
      | Empty -> ()
      | Fixed n | Mutable n ->
          let color =
            if is_invalid ~row ~col
            then error_color
            else if board.(row).(col) = Fixed n
            then fixed_color
            else mutable_color
          in
          draw_text ctx (string_of_int n) x y color (cell_size *. 0.5)
    done
  done ;

  (* Draw grid *)
  draw_grid ctx

(* Convert screen position to board position *)
let screen_to_board_pos x y =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let board_x = int_of_float ((fy -. margin) /. cell_size) in
  let board_y = int_of_float ((fx -. margin) /. cell_size) in
  if board_x >= 0 && board_x < 9 && board_y >= 0 && board_y < 9
  then Some (board_x, board_y)
  else None

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

let is_empty cell = match cell with Empty -> true | _ -> false
let is_fixed cell = match cell with Fixed _ -> true | _ -> false

(* Helper function to check if a number exists in given range *)
let value_in_row board ~row ~value =
  let rec check col =
    if col >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (col + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (col + 1)
  in
  check 0

let value_in_col board ~col ~value =
  let rec check row =
    if row >= 9
    then false
    else
      match board.(row).(col) with
      | Empty -> check (row + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check (row + 1)
  in
  check 0

let value_in_box board ~row ~col ~value =
  let box_row = row / 3 * 3 in
  let box_col = col / 3 * 3 in
  let rec check_box r c =
    if r >= box_row + 3
    then false
    else if c >= box_col + 3
    then check_box (r + 1) box_col
    else
      match board.(r).(c) with
      | Empty -> check_box r (c + 1)
      | (Fixed n | Mutable n) when n = value -> true
      | _ -> check_box r (c + 1)
  in
  check_box box_row box_col

(* Check if a row, column, or box contains all digits 1-9 without allocating lists *)
let is_valid_set_cells arr =
  let seen = Array.make 10 false in
  let valid = ref true in
  Array.iter (function
    | Fixed n | Mutable n when n >= 1 && n <= 9 ->
        if seen.(n) then valid := false else seen.(n) <- true
    | _ -> valid := false
  ) arr;
  !valid

(* Check if a number exists in a row *)
let number_in_row board row num =
  Array.exists (fun cell -> match cell with Fixed n | Mutable n when n = num -> true | _ -> false) board.(row)

(* Check if a number exists in a column *)
let number_in_col board col num =
  Array.exists (fun row -> match row.(col) with Fixed n | Mutable n when n = num -> true | _ -> false) board

(* Check if a number exists in a 3x3 box *)
let number_in_box board row col num =
  let start_row, start_col = (row / 3) * 3, (col / 3) * 3 in
  let exists = ref false in
  for r = start_row to start_row + 2 do
    for c = start_col to start_col + 2 do
      match board.(r).(c) with
      | Fixed n | Mutable n when n = num -> exists := true
      | _ -> ()
    done
  done;
  !exists

(* Check if a number is valid in a cell *)
let is_valid_number board row col num =
  num >= 1 && num <= 9 &&
  not (number_in_row board row num) &&
  not (number_in_col board col num) &&
  not (number_in_box board row col num)

(* Optimized function to check if the board is solved *)
  let is_board_solved board =
    (* Quick fail: If any cell is empty, board is not solved *)
    if Array.exists (Array.exists ((=) Empty)) board then false
    else
      (* Check all rows *)
      let rec check_rows row =
        row > 8 || (is_valid_set_cells board.(row) && check_rows (row + 1))
      in
      check_rows 0 &&
      (* Check all columns *)
      let rec check_cols col =
        col > 8 || (is_valid_set_cells (Array.init 9 (fun row -> board.(row).(col))) && check_cols (col + 1))
      in
      check_cols 0 &&
      (* Check all 3x3 boxes *)
      let rec check_boxes box_row box_col =
        if box_row > 2 then true
        else if box_col > 2 then check_boxes (box_row + 1) 0
        else
          let box_values = Array.init 9 (fun i ->
            let r = box_row * 3 + (i / 3) in
            let c = box_col * 3 + (i mod 3) in
            board.(r).(c)
          ) in
          is_valid_set_cells box_values && check_boxes box_row (box_col + 1)
      in
      check_boxes 0 0
