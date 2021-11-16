(**
A module to perform traditional iterative dataflow analysis to find pairs of
interfering variables.
*)

(** Represents a set of use/def equations over a set of instructions *)
type t

(**
[create num_instructions num_variables] creates a set of liveness equations.

Instructions and variables are referenced in a zero-indexed manner by other functions.
*)
val create : int -> int -> t

(** [add_use eqn inst_idx var_idx] indicates a use of a variable by an instruction *)
val add_use : t -> int -> int -> unit

(** [add_def eqn inst_idx var_idx] indicates a def of a variable by an instruction *)
val add_def : t -> int -> int -> unit

(**
[add_def eqn pred_inst_idx succ_inst_idx] indicates that [succ_inst_idx] is a
successor of [pred_inst_idx].

All successors must be explicitly added. (For example, there is no assumption
that instruction [idx+1] is a successor of instruction [idx].)
*)
val add_succ : t -> int -> int -> unit

(**
Runs the dataflow analysis and calls the provided callback function with pairs
of interfering variables.

The callback should expect to be called with many duplicate pairs.
*)
val solve : t -> (int -> int -> unit) -> unit

(** Run unit tests. Throws on error. *)
val tests : unit -> unit