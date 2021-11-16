let rec shift_needed_to_clear x = if x = 0 then 0 else 1 + shift_needed_to_clear (x lsr 1)

module Bitarray2 = struct
    type t = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array2.t

    let create rows cols =
        let num_column_nativeints = (cols + (Nativeint.size - 1)) / Nativeint.size in
        let bitary = Bigarray.Array2.create Bigarray.nativeint Bigarray.c_layout rows num_column_nativeints in
        Bigarray.Array2.fill bitary 0n;
        bitary

    let bit_index_mask = Nativeint.size - 1
    let word_index_shift = shift_needed_to_clear bit_index_mask

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
    let max_inst_idx = Bigarray.Array2.dim1 live_out - 1 in
    let max_word_idx = Bigarray.Array2.dim2 live_out - 1 in
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

let random_equations () = begin
    let num_vars = 100 in
    let num_insts = 200 in
    let num_uses = num_insts * 2 in
    let num_defs = num_insts in
    let num_jumps = num_insts / 10 in
    let eqn = create num_vars num_insts in

    for inst_idx = 0 to num_insts - 2 do
        add_succ eqn inst_idx (inst_idx + 1);
    done;
    for i = 1 to num_jumps do
        add_succ eqn (Random.int (num_insts - 1)) (Random.int (num_insts - 1));
    done;
    for i = 1 to num_uses do
        add_use eqn (Random.int num_insts) (Random.int num_vars);
    done;
    for i = 1 to num_defs do
        add_def eqn (Random.int num_insts) (Random.int num_vars);
    done;

    eqn;
end

(* Probably there is some clever memoization or dynamic programming way to
check the liveness analysis, but that gets uncomfortably close to how the
values were calculated in the first place, so we keep it simple and just do a
random walk from an arbitrary instruction and assert that any uses we find
were in "live_out" to begin with or in definitions we passed over. *)
let check_live_out eqn live_out starting_inst_idx = begin
    let max_word_idx = Bigarray.Array2.dim2 live_out - 1 in
    let currently_live = Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout (max_word_idx + 1) in
    let rec go inst_idx timeout =
        if timeout = 0 || eqn.successors.(inst_idx) = [] then
            (* Done *)
            ()
        else begin
            for i = 0 to max_word_idx do
                (* All uses must be in currently_live *)
                assert ((Nativeint.logand eqn.uses.{inst_idx, i} currently_live.{i}) = eqn.uses.{inst_idx, i});

                (* Update currently live based on defs *)
                currently_live.{i} <- Nativeint.logor currently_live.{i} eqn.defs.{inst_idx, i};
            done;
            let succ_list = eqn.successors.(inst_idx) in

            (* Explore random successor *)
            go (List.nth succ_list (Random.int (List.length succ_list))) (timeout - 1);
        end;
    in

    for i = 0 to max_word_idx do
        currently_live.{i} <- live_out.{starting_inst_idx, i};
    done;

    (* Prime the pump by going to an arbitrary successor of starting_inst_idx.
    (Otherwise we may find uses that aren't in currently_live, since we
    seeded currently_live from "live out", not "live in") *)
    match eqn.successors.(starting_inst_idx) with
        | [] -> ()
        | next_inst_idx :: _ -> go next_inst_idx 1000;
end

let test_find_live_out () = begin
    let num_runs = 10 in
    let num_tests = 10 in
    for i = 1 to num_runs do
        let eqn = random_equations () in
        let live_out = find_live_out eqn in
        for j = 1 to num_tests do
            check_live_out eqn live_out (Random.int eqn.num_insts);
        done;
    done;
end

let tests () = begin
    test_find_live_out ();
end