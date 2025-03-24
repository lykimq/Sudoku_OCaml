let debug_mode = ref false

let debug fmt =
  if !debug_mode
  then
    Printf.ksprintf
      (fun s ->
        Printf.printf "%s\n" s ;
        flush stdout)
      fmt
  else Printf.ksprintf (fun _ -> ()) fmt

let debug_cell i cell =
  debug "Cell %d: %s\n" i
    (match cell with
    | Board.Empty -> "Empty"
    | Board.Fixed n -> Printf.sprintf "Fixed %d" n
    | Board.Mutable n -> Printf.sprintf "Mutable %d" n) ;
  flush stdout
