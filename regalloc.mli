include module type of Regalloc_types

val allocate : int -> reg_class array -> inst list -> allocation_result

val tests : unit -> unit