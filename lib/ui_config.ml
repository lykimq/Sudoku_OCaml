(* UI configuration constants for consistent visual design and layout.

   Functional Purpose: Centralizes all UI styling and layout parameters for
   maintainable and consistent visual design across the application.

   Design Choice: Global constants over hardcoded values throughout codebase -
   Single source of truth for design decisions - Easy theme modifications and
   color scheme changes - Prevents scattered magic numbers in UI code - Enables
   compile-time validation of color values

   Alternative: Could use external configuration files, CSS-like system, or
   runtime-configurable themes *)

(* Color scheme using RGB tuples for GTK+ Cairo rendering.

   Functional Purpose: Defines visual identity and accessibility through
   carefully chosen color palette with semantic meaning.

   Design Choice: RGB float tuples (0.0-1.0) over integer RGB (0-255) - Direct
   compatibility with Cairo graphics API - Avoids runtime conversion overhead
   during rendering - Float precision allows for subtle color gradients

   Color Psychology and Accessibility: - White background: Clean, professional,
   high contrast - Black text/grid: Maximum readability, universal accessibility
   - Blue for mutable: Indicates user-editable content - Yellow selection: High
   visibility without aggressive contrast - Red errors: Universal error
   indication, attention-grabbing - Gray hints: Subtle assistance without
   overwhelming

   Alternative: Could use HSV color space, named color constants, or
   accessibility-optimized palettes *)
let background_color = (1., 1., 1.)
(* white *)

let grid_color = (0., 0., 0.) (* black *)
let fixed_color = (0., 0., 0.)
let mutable_color = (0., 0., 1.) (* blue *)
let selected_color = (0.8, 0.8, 0.1) (* yellow *)
let error_color = (1., 0.2, 0.2) (* red *)
let hint_color = (0.5, 0.5, 0.5) (* gray color for hints *)

(* Layout and sizing constants optimized for usability and visual hierarchy.

   Functional Purpose: Establishes spatial relationships and sizing that
   optimize for both desktop and touch interfaces.

   Design Choice: Fixed pixel sizing over relative/percentage sizing -
   Consistent experience across different screen sizes - Predictable layout
   calculations for grid positioning - Optimal cell size for both mouse
   precision and touch interaction

   Sizing Rationale: - 60px cells: Large enough for touch, small enough for
   desktop visibility - 20px margins: Provides breathing room without wasting
   space - Total size calculation: Ensures precise window sizing

   Alternative: Could use responsive sizing, DPI-aware scaling, or
   user-configurable sizes *)
let cell_size = 60.0
let board_size = 9. *. cell_size
let margin = 20.0
let total_size = int_of_float (board_size +. (2.0 *. margin))
