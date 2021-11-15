type t

val create : int -> t
val precolor : t -> int -> int -> unit
val interferes_with : t -> int -> int -> unit
val allocate : t -> int array

val tests : unit -> unit