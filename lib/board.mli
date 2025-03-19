type cell =
  | Empty
  | Fixed of int
  | Mutable of int

type t = cell array array

val create : unit -> t
val of_array : int array array -> t
val to_array : t -> int array array
val pp : Format.formatter -> t -> unit
