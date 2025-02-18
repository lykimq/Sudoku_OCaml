open Cairo

type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

(* GTK+ related constants *)
let cell_size = 60.0
let board_size = 9. *. cell_size
let margin = 20.0
let total_size = int_of_float (board_size +. (2.0 *. margin))

(* Colors *)
let background_color = (1., 1., 1.) (* white *)
let grid_color = (0., 0., 0.) (* black *)
let fixed_color = (0., 0., 0.)
let mutable_color = (0., 0., 1.) (* blue *)
let selected_color = (0.8, 0.8, 0.1) (* yellow *)
let error_color = (1., 0.2, 0.2) (* red *)
let create () = Array.make_matrix 9 9 Empty

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
      (match selected with
      | Some (sr, sc) when sr = row && sc = col ->
          let sr, sg, sb = selected_color in
          Cairo.set_source_rgb ctx sr sg sb ;
          Cairo.rectangle ctx x y ~w:cell_size ~h:cell_size ;
          Cairo.fill ctx
      | _ ->
          let br, bg, bb = background_color in
          Cairo.set_source_rgb ctx br bg bb ;
          Cairo.rectangle ctx x y ~w:cell_size ~h:cell_size ;
          Cairo.fill ctx) ;

      (* Draw cell content *)
      match board.(row).(col) with
      | Empty -> ()
      | Fixed n | Mutable n ->
          let color =
            if board.(row).(col) = Fixed n then fixed_color else mutable_color
          in
          let r, g, b = color in
          Cairo.set_source_rgb ctx r g b ;
          Cairo.select_font_face ctx "Sans" ~weight:Cairo.Bold ;
          Cairo.set_font_size ctx (cell_size *. 0.5) ;
          Cairo.move_to ctx (x +. 15.0) (y +. 35.0) ;

          let text = string_of_int n in
          let extents = Cairo.text_extents ctx text in
          let text_x = x +. ((cell_size -. extents.width) /. 2.0) in
          let text_y = y +. ((cell_size +. extents.height) /. 2.0) in
          Cairo.move_to ctx text_x text_y ;
          Cairo.show_text ctx text
    done
  done ;

  (* Draw grid *)
  let gr, gg, gb = grid_color in
  Cairo.set_source_rgb ctx gr gg gb ;

  (* Draw lines *)
  for i = 0 to 9 do
    let line_width = if i mod 3 = 0 then 2.0 else 1.0 in
    Cairo.set_line_width ctx line_width ;

    (* Vertical lines *)
    let x = margin +. (float_of_int i *. cell_size) in
    Cairo.move_to ctx x margin ;
    Cairo.line_to ctx x (margin +. board_size) ;
    Cairo.stroke ctx ;

    (* Horizontal lines *)
    let y = margin +. (float_of_int i *. cell_size) in
    Cairo.move_to ctx margin y ;
    Cairo.line_to ctx (margin +. board_size) y ;
    Cairo.stroke ctx
  done ;

  (* Draw outer border with thicker line *)
  Cairo.set_line_width ctx 3.0 ;
  Cairo.rectangle ctx margin margin ~w:board_size ~h:board_size ;
  Cairo.stroke ctx

let screen_to_board_pos x y =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let board_x = int_of_float ((fy -. margin) /. cell_size) in
  let board_y = int_of_float ((fx -. margin) /. cell_size) in
  if board_x >= 0 && board_x < 9 && board_y >= 0 && board_y < 9
  then Some (board_x, board_y)
  else None

let of_array array =
  let board = create () in
  for i = 0 to 8 do
    for j = 0 to 8 do
      board.(i).(j) <- (match array.(i).(j) with 0 -> Empty | n -> Fixed n)
    done
  done ;
  board

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
