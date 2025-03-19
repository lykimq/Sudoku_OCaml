
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
