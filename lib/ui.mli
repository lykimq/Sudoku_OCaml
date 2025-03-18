val start_ui :
  ?debug:bool ->
  ?key_press_handler:(int -> int option) ->
  ?click_handler:(int -> int -> unit) ->
  int array array ->
  unit

val update_board : Board.t -> unit
val refresh_display : unit -> unit
val reset_game_state : unit -> unit
