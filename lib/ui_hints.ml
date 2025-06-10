(* Draws a single hint number within a specific position in a Sudoku cell.

   Functional Purpose: Renders individual hint numbers in a 3x3 sub-grid layout
   within each cell for comprehensive move assistance.

   Design Choice: 3x3 sub-grid layout for hint organization - Maps hint position
   (0-8) to visual grid position within cell - Consistent spatial relationship
   between hint number and position - Intuitive layout: position 1 appears in
   top-left, 9 in bottom-right - Small font size to fit multiple hints without
   overcrowding

   Algorithm: Position mapping and coordinate calculation 1. Convert linear
   index (0-8) to 2D grid position: - hint_row = i / 3 (integer division gives
   row 0, 1, or 2) - hint_col = i mod 3 (modulo gives column 0, 1, or 2) 2.
   Calculate pixel coordinates within cell: - Divide cell into 3x3 sub-grid
   (cell_size / 3 per sub-cell) - Add padding for visual spacing and readability
   3. Position cursor and render with small font Time: O(1) per hint number

   Layout rationale: - 3x3 grid matches Sudoku box structure (familiar to users)
   - Padding values (5.0, 15.0) provide visual breathing room - Small font size
   (20% of cell) ensures readability without dominance

   Alternative: Could use circular layout, list format, or color coding *)
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

(* Draws all hint numbers on the Sudoku board with optimized rendering.

   Functional Purpose: Renders comprehensive hint overlay showing all possible
   valid moves for every empty cell on the board.

   Design Choice: Batch rendering with consistent styling - Single font
   configuration for all hints (performance optimization) - Conditional
   rendering only for cells with hints (efficiency) - Gray color scheme for
   subtle assistance without overwhelming - Proportional font sizing (20% of
   cell size) for readability

   Algorithm: Nested iteration with conditional rendering 1. Configure global
   hint styling (color, font, size) - O(1) 2. For each board position (9x9): -
   Skip cells without hints (empty list check) - Calculate cell screen
   coordinates - For each hint in cell: render at appropriate sub-position 3.
   Total: O(cells_with_hints × hints_per_cell) - Best case: O(1) for solved
   board (no hints) - Worst case: O(9 × 81) = O(729) for empty board with all
   hints

   Performance optimizations: - Early skip for cells without hints (most cells
   after some progress) - Single font configuration vs per-hint configuration -
   List.iteri for efficient index-based positioning - Proportional sizing
   calculation done once

   Visual design principles: - Gray color: Subtle assistance, doesn't compete
   with main numbers - Normal font weight: Less prominent than bold main numbers
   - Small size: Fits multiple hints, maintains hierarchy - Consistent
   positioning: Predictable hint locations

   Alternative approaches: - On-demand rendering: Only show hints for selected
   cell - Color-coded hints: Different colors for different constraint types -
   Popup hints: Show hints in separate overlay or tooltip *)
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
