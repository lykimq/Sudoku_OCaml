let background_color = (1., 1., 1.)
(* white *)

let grid_color = (0., 0., 0.) (* black *)
let fixed_color = (0., 0., 0.)
let mutable_color = (0., 0., 1.) (* blue *)
let selected_color = (0.8, 0.8, 0.1) (* yellow *)
let error_color = (1., 0.2, 0.2) (* red *)
let hint_color = (0.5, 0.5, 0.5) (* gray color for hints *)

(* Sizing Rationale: - 60px cells: Large enough for touch, small enough for
   desktop visibility - 20px margins: Provides breathing room without wasting
   space - Total size calculation: Ensures precise window sizing *)
let cell_size = 60.0
let board_size = 9. *. cell_size
let margin = 20.0
let total_size = int_of_float (board_size +. (2.0 *. margin))
