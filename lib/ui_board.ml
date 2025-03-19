open Board
open Configure_ui

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
            if Invalid_cells.is_invalid ~row ~col
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

(* Draw the board with hints *)
let draw_board_with_hints drawing_area board_ref =
  drawing_area#event#connect#expose ~callback:(fun _ev ->
      let ctxt = Cairo_gtk.create drawing_area#misc#window in
      (* Draw the main board *)
      draw_board ctxt !board_ref !Ui_state.selected ;
      (* Draw the hints if the show hints option is on *)
      if !Ui_state.show_hints
      then begin
        (* If the hints are empty, compute them *)
        if
          Array.for_all
            (Array.for_all (fun x -> x = []))
            !Ui_state.current_hints
        then Ui_state.current_hints := Hints.compute_all_hints !board_ref ;
        (* Draw the hints *)
        Hints.draw_hints ctxt !Ui_state.current_hints
      end ;
      true)

(* Convert screen position to board position *)
let screen_to_board_pos x y =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let board_x = int_of_float ((fy -. margin) /. cell_size) in
  let board_y = int_of_float ((fx -. margin) /. cell_size) in
  if board_x >= 0 && board_x < 9 && board_y >= 0 && board_y < 9
  then Some (board_x, board_y)
  else None
