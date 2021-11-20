include module type of Regalloc_types

val allocate : inst list -> int -> (virt_reg, phys_reg) Hashtbl.t -> phys_reg array

val tests : unit -> unit