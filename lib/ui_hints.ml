(* Draws a single hint number within a specific position in a Sudoku cell.

   - x: The x-coordinate of the top-left corner of the cell

   - y: The y-coordinate of the top-left corner of the cell

   - n: The number to draw (1-9)

   - i: The position index (0-8) within the 3x3 sub-grid inside a cell

   - hint_size: The font size for the hint number *)
let draw_hint_number ctxt x y n i hint_size =
  (* Determine row (0, 1, or 2) within the 3x3 grid *)
  let hint_row = i / 3 in
  (* Determine column (0, 1, or 2) within the 3x3 grid *)
  let hint_col = i mod 3 in

  (* Calculate exact x, y coordinates for this hint number, 5. and 15. are the
     padding values *)
  let hint_x =
    x +. (float_of_int hint_col *. Ui_config.cell_size /. 3.) +. 5.
  in
  let hint_y =
    y +. (float_of_int hint_row *. Ui_config.cell_size /. 3.) +. 15.
  in

  (* Position the drawing cursor *)
  Cairo.move_to ctxt hint_x hint_y ;

  (* Set the font size for the hint (smaller than regular numbers) *)
  Cairo.set_font_size ctxt (float_of_int hint_size) ;

  (* Draw the hint number *)
  Cairo.show_text ctxt (string_of_int n)

(* Draws all hint numbers on the Sudoku board. *)
let draw_hints ctxt (hints : int list array array) =
  (* Make hints much smaller - 20% of cell size *)
  let hint_size = int_of_float (Ui_config.cell_size *. 0.2) in

  (* Set hint color from predefined hint_color *)
  let r, g, b = Ui_config.hint_color in
  Cairo.set_source_rgb ctxt r g b ;

  (* Set font face to Sans with normal weight *)
  Cairo.select_font_face ctxt "Sans" ~weight:Cairo.Normal ;

  (* Iterate through each cell in the 9x9 hints array *)
  Array.iteri
    (fun row row_array ->
      Array.iteri
        (fun col hints ->
          (* Only draw hints if there are valid numbers for this cell *)
          if hints <> []
          then
            (* Calculate the top-left position of this cell *)
            let x =
              Ui_config.margin +. (float_of_int col *. Ui_config.cell_size)
            in
            let y =
              Ui_config.margin +. (float_of_int row *. Ui_config.cell_size)
            in

            (* Draw each hint number in its position within the cell *)
            List.iteri
              (fun i n -> draw_hint_number ctxt x y n i hint_size)
              hints)
        row_array)
    hints
