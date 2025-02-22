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
let hint_color = (0.5, 0.5, 0.5) (* gray color for hints *)

(* GTK+ related constants *)
let cell_size = 60.0
let board_size = 9. *. cell_size
let margin = 20.0
let total_size = int_of_float (board_size +. (2.0 *. margin))

(* Invalid cells *)
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

(* Draw hint number *)
let draw_hint_number ctxt x y n i hint_size =
  let hint_row = i / 3 in
  let hint_col = i mod 3 in
  let hint_x = x +. (float_of_int hint_col *. cell_size /. 3.) +. 5. in
  let hint_y = y +. (float_of_int hint_row *. cell_size /. 3.) +. 15. in
  Cairo.move_to ctxt hint_x hint_y ;
  Cairo.set_font_size ctxt (float_of_int hint_size) ;
  Cairo.show_text ctxt (string_of_int n)

(* Draw hints *)
let draw_hints cr (hints : int list array array) =
  (* Make hints much smaller - 20% of cell size *)
  let hint_size = int_of_float (cell_size *. 0.2) in
  (* Set hint color *)
  let r, g, b = hint_color in
  Cairo.set_source_rgb cr r g b ;
  Cairo.select_font_face cr "Sans" ~weight:Cairo.Normal ;

  Array.iteri
    (fun row row_array ->
      Array.iteri
        (fun col hints ->
          if hints <> []
          then
            let x = margin +. (float_of_int col *. cell_size) in
            let y = margin +. (float_of_int row *. cell_size) in
            (* Draw each possible number in a grid within the cell *)
            List.iteri (fun i n -> draw_hint_number cr x y n i hint_size) hints)
        row_array)
    hints

let is_empty cell = match cell with Empty -> true | _ -> false
let is_fixed cell = match cell with Fixed _ -> true | _ -> false

(* Helper function to filter hints *)
let filter_hints original_board (hints_board : int list array array) :
    int list array array =
  Array.mapi
    (fun row row_array ->
      Array.mapi
        (fun col value ->
          if
            is_empty original_board.(row).(col)
            && not (is_fixed original_board.(row).(col))
          then value
          else [])
        row_array)
    hints_board

(* Helper function to check if a number exists in given range *)
let exists_in_range check_fn num =
  let rec check i =
    if i >= 9
    then true
    else
      match check_fn i with
      | Empty -> check (i + 1)
      | Fixed n | Mutable n -> if n = num then false else check (i + 1)
  in
  check 0

(* Helper function to check if a number exists in the 3x3 box *)
let valid_in_box board row col num =
  let box_row, box_col = (row / 3 * 3, col / 3 * 3) in
  let rec check_box r c =
    if r >= box_row + 3
    then true
    else if c >= box_col + 3
    then check_box (r + 1) box_col
    else if r = row && c = col
    then check_box r (c + 1)
    else
      match board.(r).(c) with
      | Empty -> check_box r (c + 1)
      | Fixed n | Mutable n -> if n = num then false else check_box r (c + 1)
  in
  check_box box_row box_col

(* Helper function to check if a number is valid in a cell *)
let is_valid_number board row col num =
  (* Check row *)
  let valid_in_row =
    exists_in_range (fun i -> if i = col then Empty else board.(row).(i))
  in
  (* Check column *)
  let valid_in_column =
    exists_in_range (fun i -> if i = row then Empty else board.(i).(col))
  in
  (* Check 3x3 box *)
  let valid_in_box = valid_in_box board row col num in
  valid_in_row num && valid_in_column num && valid_in_box

(* Helper function to check if a board is valid *)
let is_valid board =
  let valid_cell row col =
    match board.(row).(col) with
    | Empty -> true
    | Fixed n | Mutable n -> is_valid_number board row col n
  in
  let rec check_all_cells row col =
    if row >= 9
    then true
    else if col >= 9
    then check_all_cells (row + 1) 0
    else if valid_cell row col
    then check_all_cells row (col + 1)
    else false
  in
  check_all_cells 0 0

(* Helper function to check if a board is full *)
let is_full board =
  let rec check_all_cells row col =
    if row >= 9
    then true
    else if col >= 9
    then check_all_cells (row + 1) 0
    else
      match board.(row).(col) with
      | Empty -> false
      | _ -> check_all_cells row (col + 1)
  in
  check_all_cells 0 0
