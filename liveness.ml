let rec index_of_highest_bit_set x = if x = 1 then 0 else 1 + index_of_highest_bit_set (x lsr 1)

module Bitarray2 = struct
    type t = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array2.t

    let create rows cols =
        let num_column_nativeints = (cols + (Nativeint.size - 1)) / Nativeint.size in
        let bitary = Bigarray.Array2.create Bigarray.nativeint Bigarray.c_layout rows num_column_nativeints in
        Bigarray.Array2.fill bitary 0n;
        bitary

    let bit_index_mask = Nativeint.size - 1
    let word_index_shift = index_of_highest_bit_set bit_index_mask

    let set (bitary : t) row col =
        let col_bit_idx = col land bit_index_mask in
        let col_word_idx = col lsr word_index_shift in
        bitary.{row, col_word_idx} <- Nativeint.logor (bitary.{row, col_word_idx}) (Nativeint.shift_left 1n col_bit_idx)

    let is_set (bitary : t) row col =
        let col_bit_idx = col land bit_index_mask in
        let col_word_idx = col lsr word_index_shift in
        Nativeint.(logand (shift_right bitary.{row, col_word_idx} col_bit_idx) 1n) = 1n
end

type t = {
    num_vars: int;
    num_insts : int;
    uses : Bitarray2.t;
    defs : Bitarray2.t;
    successors : int list array;
}

let create num_vars num_insts = {
    num_vars;
    num_insts;
    uses = Bitarray2.create num_insts num_vars;
    defs = Bitarray2.create num_insts num_vars;
    successors = Array.make num_insts [];
}

let add_use eqn inst_idx var_idx = Bitarray2.set eqn.uses inst_idx var_idx
let add_def eqn inst_idx var_idx = Bitarray2.set eqn.defs inst_idx var_idx
let add_succ eqn from_inst_idx to_inst_idx = Array.set eqn.successors from_inst_idx (to_inst_idx :: Array.get eqn.successors from_inst_idx)

let find_live_out eqn = begin
    let live_out = Bitarray2.create eqn.num_insts eqn.num_vars in
    let max_word_idx = Bigarray.Array2.dim2 eqn.uses - 1 in
    let num_updates = ref Int.max_int in

    (* Do interative dataflow analysis to calculate the "live out" set of
    every instruction *)
    while !num_updates > 0 do
        num_updates := 0;

        (* For every instruction in reverse order *)
        for inst_idx = (eqn.num_insts - 1) downto 0 do
            let succ = ref (Array.get eqn.successors inst_idx) in

            (* Loop over successors *)
            while !succ <> [] do
                let succ_idx = List.hd !succ in
                succ := List.tl !succ;

                for word_idx = 0 to max_word_idx do
                    (* Calculate "live in" of successor instruction *)
                    let succ_use = eqn.uses.{succ_idx, word_idx} in
                    let succ_def = eqn.defs.{succ_idx, word_idx} in
                    let succ_live_out = live_out.{succ_idx, word_idx} in
                    (* The live in set is the variables that an instruction
                    uses, plus its "live out" set minus any variables that it
                    defines.*)
                    let succ_live_in = Nativeint.(logor succ_use (logand succ_live_out (lognot succ_def))) in

                    (* Union (ie, bitwise OR) the "live in" set of successor
                    instruction into our "live out" set *)
                    let old_live_out = live_out.{inst_idx, word_idx} in
                    let new_live_out = Nativeint.logor old_live_out succ_live_in in
                    if new_live_out <> old_live_out then begin
                        num_updates := !num_updates + 1;
                        live_out.{inst_idx, word_idx} <- new_live_out;
                    end;
                done;
            done;
        done;
    done;

    live_out
end

let call_with_interferers f (live_out : Bitarray2.t) = begin
    let max_inst_idx = Bigarray.Array2.dim1 live_out in
    let max_word_idx = Bigarray.Array2.dim2 live_out in
    let max_bit_idx = (max_word_idx * Nativeint.size) - 1 in

    for inst_idx = 0 to max_inst_idx do
        for bit_a_idx = 0 to max_bit_idx do
            if Bitarray2.is_set live_out inst_idx bit_a_idx then begin
                for bit_b_idx = bit_a_idx to max_bit_idx do
                    if Bitarray2.is_set live_out inst_idx bit_b_idx then f bit_a_idx bit_b_idx;
                done;
            end;
        done;
    done;
end

let solve eqn f = find_live_out eqn |> call_with_interferers f