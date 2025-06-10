open Board
open Ui_config

(* Draws cell background with specified color using Cairo graphics.

   Functional Purpose: Provides uniform cell background rendering with
   configurable colors for different cell states and user feedback.

   Design Choice: Separate background function over inline drawing - Reusable
   for different cell states (selected, normal, error) - Consistent cell sizing
   and positioning across all states - Clear separation between background and
   content rendering - Easy color customization for themes and accessibility

   Algorithm: Cairo rectangle fill operation 1. Set RGB color from tuple - O(1)
   2. Define rectangle with cell dimensions - O(1) 3. Fill rectangle with
   current color - O(1) Time: O(1) per cell, O(n²) for full board

   Alternative: Could use pre-rendered textures or gradient fills *)
let drawing_background ctxt color x y =
  let r, g, b = color in
  Cairo.set_source_rgb ctxt r g b ;
  Cairo.rectangle ctxt x y ~w:cell_size ~h:cell_size ;
  Cairo.fill ctxt

(* Draws centered text in cell with specified color and font size.

   Functional Purpose: Renders numbers and text with precise centering and
   consistent typography across all board cells.

   Design Choice: Mathematical centering over approximate positioning - Cairo
   text_extents for precise text measurement - Pixel-perfect centering for
   professional appearance - Configurable font size for different content types
   (numbers vs hints) - Bold font weight for better readability

   Algorithm: Text measurement and centering calculation 1. Set color and font
   properties - O(1) 2. Measure text extents (width, height) - O(text_length) 3.
   Calculate centered position: cell_center - text_center/2 4. Position cursor
   and render text - O(text_length) Time: O(text_length) per text, typically
   O(1) for single digits

   Typography choices: - Sans font: Clean, modern, highly readable - Bold
   weight: Better visibility on various backgrounds - Configurable size: Adapts
   to content importance (numbers vs hints)

   Alternative: Could use pre-rendered glyph cache or bitmap fonts *)
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

(* Draws Sudoku grid with thick lines for 3x3 box boundaries.

   Functional Purpose: Renders the visual structure of Sudoku constraints with
   clear hierarchy between cell boundaries and box boundaries.

   Design Choice: Variable line width for visual hierarchy - Thick lines (2px)
   for 3x3 box boundaries (major constraints) - Thin lines (1px) for individual
   cell boundaries (minor grid) - Extra thick border (3px) for overall board
   boundary - Mathematical line positioning for pixel-perfect alignment

   Algorithm: Hierarchical grid rendering 1. For each grid line (0-9): -
   Determine line thickness based on position (mod 3) - Draw vertical line from
   top to bottom - Draw horizontal line from left to right 2. Draw outer border
   with thickest line Time: O(grid_lines) = O(1) for fixed 9x9 grid

   Visual hierarchy rationale: - Box boundaries: Most important Sudoku
   constraints - Cell boundaries: Individual cell separation - Outer border:
   Overall game area definition

   Alternative: Could use different colors instead of line weights, or SVG-style
   grid templates *)
let draw_grid ctxt =
  let gr, gg, gb = grid_color in
  Cairo.set_source_rgb ctxt gr gg gb ;

  (* Thick lines for 3x3 boxes, thin lines for cells *)
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

  (* Outer border *)
  Cairo.set_line_width ctxt 3.0 ;
  Cairo.rectangle ctxt margin margin ~w:board_size ~h:board_size ;
  Cairo.stroke ctxt

(* Main board rendering function with cell highlighting and number display.

   Functional Purpose: Orchestrates complete board visualization including
   backgrounds, content, selection feedback, and error indication.

   Design Choice: Layered rendering approach for clear visual hierarchy - Layer
   1: Background clearing (clean slate) - Layer 2: Cell backgrounds (selection
   highlighting) - Layer 3: Cell content (numbers with color coding) - Layer 4:
   Grid overlay (structural lines) - Separates concerns for maintainable
   rendering pipeline

   Algorithm: Multi-pass rendering with state-based styling 1. Clear entire
   background - O(1) 2. For each cell (9x9): - Calculate screen position from
   grid coordinates - Determine background color based on selection state -
   Render background with appropriate color - Render content with
   state-appropriate color coding 3. Overlay grid structure - O(1) Total: O(n²)
   where n=9 for Sudoku

   Color coding system: - Fixed cells: Black (puzzle clues, immutable) - Mutable
   cells: Blue (user entries, editable) - Invalid cells: Red (constraint
   violations, error feedback) - Selected cell: Yellow background (current
   focus) - Normal cells: White background (default state)

   Performance considerations: - Single pass through all cells for efficiency -
   State-based color selection avoids redundant calculations - Layered approach
   enables partial redraws in future optimizations

   Alternative: Could use retained-mode graphics, dirty rectangle tracking, or
   hardware-accelerated rendering *)
let draw_board (ctx : Cairo.context) board selected =
  (* Clear background *)
  let br, bg, bb = background_color in
  Cairo.set_source_rgb ctx br bg bb ;
  Cairo.paint ctx ;

  (* Render each cell *)
  for row = 0 to 8 do
    for col = 0 to 8 do
      let x = margin +. (float_of_int col *. cell_size) in
      let y = margin +. (float_of_int row *. cell_size) in

      (* Cell background (highlight selected cell) *)
      let bg_color =
        match selected with
        | Some (sr, sc) when sr = row && sc = col -> selected_color
        | _ -> background_color
      in
      drawing_background ctx bg_color x y ;

      (* Cell content with appropriate color coding *)
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

  draw_grid ctx

(* Converts screen coordinates to board position (row, col).

   Functional Purpose: Translates mouse click coordinates to logical board
   positions for user interaction handling.

   Design Choice: Option type for safe coordinate conversion - Returns None for
   clicks outside board area (graceful handling) - Returns Some (row, col) for
   valid board positions - Handles margin offset and cell size scaling
   automatically - Type-safe coordinate validation prevents array bounds errors

   Algorithm: Inverse coordinate transformation with bounds checking 1. Convert
   integer screen coordinates to float 2. Subtract margin offset to get
   board-relative coordinates 3. Divide by cell_size to get grid coordinates 4.
   Convert back to integers (truncation gives cell index) 5. Validate bounds
   (0-8 for both row and col) 6. Return Option type based on validation result
   Time: O(1) coordinate transformation and validation

   Coordinate system mapping: - Screen: (x, y) in pixels from top-left - Board:
   (row, col) in grid cells from top-left - Transformation:
   (x-margin)/cell_size, (y-margin)/cell_size - Note: x maps to col, y maps to
   row (screen vs matrix conventions)

   Alternative: Could throw exceptions on invalid coordinates or return special
   "invalid" coordinates *)
let screen_to_board_pos x y =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let board_x = int_of_float ((fy -. margin) /. cell_size) in
  let board_y = int_of_float ((fx -. margin) /. cell_size) in
  if board_x >= 0 && board_x < 9 && board_y >= 0 && board_y < 9
  then Some (board_x, board_y)
  else None
