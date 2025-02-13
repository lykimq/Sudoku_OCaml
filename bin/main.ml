open GMain
open Soduku_OCaml

let sample_board =
  [|
    [|5; 3; 0; 0; 7; 0; 0; 0; 0|];
    [|6; 0; 0; 1; 9; 5; 0; 0; 0|];
    [|0; 9; 8; 0; 0; 0; 0; 6; 0|];
    [|8; 0; 0; 0; 6; 0; 0; 0; 3|];
    [|4; 0; 0; 8; 0; 3; 0; 0; 1|];
    [|7; 0; 0; 0; 2; 0; 0; 0; 6|];
    [|0; 6; 0; 0; 0; 0; 2; 8; 0|];
    [|0; 0; 0; 4; 1; 9; 0; 0; 5|];
    [|0; 0; 0; 0; 8; 0; 0; 7; 9|];
  |]

let () =
  (* initialize gtk *)
  let _ = GMain.init () in

  (* Create window *)
  let window =
    GWindow.window ~title:"Sudoku" ~width:Board.total_size
      ~height:Board.total_size ~resizable:true ()
  in

  let vbox = GPack.vbox ~packing:window#add () in

  (* Handle window close *)
  let _ = window#connect#destroy ~callback:quit in

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~width:Board.total_size ~height:Board.total_size
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  let _ =
    drawing_area#event#connect#configure ~callback:(fun _ ->
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in
  (* initial board *)
  let board = ref (Board.of_array sample_board) in
  let selected = ref None in

  (* Drawing callback *)
  let _ =
    drawing_area#event#connect#expose ~callback:(fun _ ->
        let cr = Cairo_gtk.create drawing_area#misc#window in
        Board.draw_board cr !board !selected ;
        true)
  in

  (* Mouse click callback *)
  let _ =
    drawing_area#event#connect#button_press ~callback:(fun ev ->
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        selected := Board.screen_to_board_pos x y ;
        GtkBase.Widget.queue_draw drawing_area#as_widget ;
        true)
  in

  (* Key press callback *)
  let _ =
    window#event#connect#key_press ~callback:(fun ev ->
        match (GdkEvent.Key.keyval ev, !selected) with
        | k, Some (row, col) when k >= 49 && k <= 57 ->
            let value = k - 48 in
            (match Board.set_cell !board ~row ~col ~value with
            | Some new_board ->
                board := new_board ;
                GtkBase.Widget.queue_draw drawing_area#as_widget
            | None -> ()) ;
            true
        | _, _ -> false)
  in

  (* Show window and start main loop *)
  window#show () ;
  GMain.main ()
