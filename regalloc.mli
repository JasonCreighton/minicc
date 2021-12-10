include module type of Regalloc_types

val allocate : int -> reg_class array -> (virt_reg * phys_reg) list -> inst list -> allocation_result

val tests : unit -> unit