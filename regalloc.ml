include Regalloc_types

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

    let iter_indexes_of_set_bits_in_row (bitary : t) row f = begin
        let num_column_nativeints = Bigarray.Array2.dim2 bitary in
        for col_word_idx = 0 to num_column_nativeints - 1 do
            let i = ref (col_word_idx * Nativeint.size) in
            let col_word = ref bitary.{row, col_word_idx} in
            while !col_word <> 0n do
                if Nativeint.logand !col_word 1n <> 0n then
                    f !i;
                i := !i + 1;
                col_word := Nativeint.shift_right !col_word 1;
            done;
        done;
    end
end

let check_coloring ifg assignments num_virtuals = begin
    for i = 0 to num_virtuals - 1 do
        for j = i + 1 to num_virtuals - 1 do
            (* If two virtuals interfere, they must not share a physical registers *)
            if ifg.{i, j} = 1 then begin
                assert (assignments.(i) <> assignments.(j));
            end;
        done;
    done;
end

let find_bit_defs_and_uses insts num_insts num_virtuals = begin
    let bit_defs = Bitarray2.create num_insts num_virtuals in
    let bit_uses = Bitarray2.create num_insts num_virtuals in
    List.iteri (fun inst_idx {op} -> 
        match op with
        | Move (dest, src) -> begin
            Bitarray2.set bit_defs inst_idx dest;
            Bitarray2.set bit_uses inst_idx src;
        end
        | Inst {defs; uses} -> begin
            List.iter (Bitarray2.set bit_defs inst_idx) defs;
            List.iter (Bitarray2.set bit_uses inst_idx) uses;
        end
    ) insts;

    (bit_defs, bit_uses)
end

let find_live_out defs uses successors num_insts num_virtuals = begin
    let live_out = Bitarray2.create num_insts num_virtuals in
    let max_word_idx = Bigarray.Array2.dim2 uses - 1 in
    let num_updates = ref Int.max_int in

    (* Do interative dataflow analysis to calculate the "live out" set of
    every instruction *)
    while !num_updates > 0 do
        num_updates := 0;

        (* For every instruction in reverse order *)
        for inst_idx = (num_insts - 1) downto 0 do
            let succ = ref (Array.get successors inst_idx) in

            (* Loop over successors *)
            while !succ <> [] do
                let succ_idx = List.hd !succ in
                succ := List.tl !succ;

                for word_idx = 0 to max_word_idx do
                    (* Calculate "live in" of successor instruction *)
                    let succ_use = uses.{succ_idx, word_idx} in
                    let succ_def = defs.{succ_idx, word_idx} in
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

let interferes_with ifg i j = begin
    ifg.{i, j} <- 1;
    ifg.{j, i} <- 1;
end

let create_ifg insts num_virtuals bit_live_out = begin
    let ifg = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout num_virtuals num_virtuals in
    List.iteri (fun inst_idx {op} ->
        match op with
        | Move (dest, src) -> Bitarray2.iter_indexes_of_set_bits_in_row bit_live_out inst_idx (fun live_out ->
            (* Moves do not cause interference between src and dest, because
            after the move the registers contain the same value and could be
            coalesced if they do not otherwise interfere. *)
            if live_out <> src then interferes_with ifg dest live_out
        )
        | Inst {defs} -> List.iter (fun def ->
            Bitarray2.iter_indexes_of_set_bits_in_row bit_live_out inst_idx (fun live_out ->
                interferes_with ifg def live_out
            )
        ) defs
    ) insts;

    ifg
end

let color_ifg ifg num_physical num_virtuals assignments = begin
    let available_phys_regs = Array.make num_physical true in
    let spills = ref [] in
    for virt_reg = 0 to num_virtuals - 1 do
        (* If virt_reg is not yet assigned *)
        if assignments.(virt_reg) = (-1) then begin
            (* Reset set of available registers *)
            Array.fill available_phys_regs 0 num_physical true;

            (* Find conflicting virtuals that have already been assigned and
            remove their physical registers from the set of avaialble
            registers. *)
            for other_reg = 0 to num_virtuals - 1 do
                let other_assignment = assignments.(other_reg) in

                if other_assignment <> (-1) && ifg.{virt_reg, other_reg} = 1 then begin
                    available_phys_regs.(assignments.(other_reg)) <- false;
                end;
            done;

            (* Assign lowest-numbered available physical register *)
            let phys_idx = ref 0 in
            let assigned = ref false in
            while not !assigned && !phys_idx < num_physical do
                if available_phys_regs.(!phys_idx) then begin
                    (* Assign register *)
                    assignments.(virt_reg) <- !phys_idx;
                    assigned := true;
                end else begin
                    phys_idx := !phys_idx + 1
                end
            done;

            (* Could not assign register, spill it *)
            if not !assigned then
                spills := virt_reg :: !spills;
        end;
    done;

    if !spills = [] then begin
        check_coloring ifg assignments num_virtuals;
        Allocation assignments
    end else begin
        Spills !spills
    end
end

let allocate num_physical reg_classes insts = begin
    let num_insts = List.length insts in
    let num_virtuals = Array.length reg_classes in

    (* Convert instructions into bitvector use/def information *)
    let bit_defs, bit_uses = find_bit_defs_and_uses insts num_insts num_virtuals in

    (* Calculate bitvector "live out" information *)
    let successors = List.map (fun {successors} -> successors) insts |> Array.of_list in
    let bit_live_out = find_live_out bit_defs bit_uses successors num_insts num_virtuals in

    (* Generate interference graph *)
    let ifg = create_ifg insts num_virtuals bit_live_out in

    (* Registers in the range [0, num_physical) are precolored to a single physical register *)
    let assignments = Array.init num_virtuals (fun i -> if i < num_physical then i else (-1)) in

    (* Add edges in the interference graph to account for register classes *)
    Array.iteri (fun reg_idx {from_idx; to_idx} ->
        for phys_reg_idx = 0 to num_physical - 1 do
            if not (phys_reg_idx >= from_idx && phys_reg_idx <= to_idx) then
                interferes_with ifg reg_idx phys_reg_idx;
        done;
    ) reg_classes;

    (* Color graph and return either assignments or spill list *)
    color_ifg ifg num_physical num_virtuals assignments;
end

let random_ifg num_virtuals num_edges = begin
    let ifg = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout num_virtuals num_virtuals in
    for i = 1 to num_edges do
        interferes_with ifg (Random.int num_virtuals) (Random.int num_virtuals)
    done;
    ifg
end

let random_insts num_virtuals = begin
    let num_insts = 500 in
    let jump_probability = 0.1 in

    List.init num_insts (fun i ->
        {
            (* Each random instruction has one def and two uses *)
            op = Inst { defs = [Random.int num_virtuals]; uses = [Random.int num_virtuals; Random.int num_virtuals] };
            successors = 
                if i = num_insts - 1 then
                    []
                else if Random.float 1.0 < jump_probability then
                    [i + 1; Random.int num_insts]
                else
                    [i + 1]
                ;
            execution_estimate = 1;
        }
    )
end

(* Probably there is some clever memoization or dynamic programming way to
check the liveness analysis, but that gets uncomfortably close to how the
values were calculated in the first place, so we keep it simple and just do a
random walk from an arbitrary instruction and assert that any uses we find
were in "live_out" to begin with or in definitions we passed over. *)
let check_live_out defs uses successors live_out starting_inst_idx = begin
    let max_word_idx = Bigarray.Array2.dim2 live_out - 1 in
    let currently_live = Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout (max_word_idx + 1) in
    let rec go inst_idx timeout =
        if timeout = 0 || successors.(inst_idx) = [] then
            (* Done *)
            ()
        else begin
            for i = 0 to max_word_idx do
                (* All uses must be in currently_live *)
                assert ((Nativeint.logand uses.{inst_idx, i} currently_live.{i}) = uses.{inst_idx, i});

                (* Update currently live based on defs *)
                currently_live.{i} <- Nativeint.logor currently_live.{i} defs.{inst_idx, i};
            done;
            let succ_list = successors.(inst_idx) in

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
    match successors.(starting_inst_idx) with
        | [] -> ()
        | next_inst_idx :: _ -> go next_inst_idx 1000;
end

let test_random_ifg () = begin
    let num_virtuals = 50 in
    (* Unless the graph is total, I think there should always be at least one
    virtual that can share another physical registers *)
    let num_physical = num_virtuals - 1 in
    let num_edges = 1000 in
    let num_runs = 10 in

    for i = 1 to num_runs do
        let ifg = random_ifg num_virtuals num_edges in
        let assignments = Array.make num_virtuals (-1) in

        (* Precolor one virtual register as physical register 0, which should
        never hurt colorability. *)
        let precolored_virtual_idx = Random.int num_virtuals in
        assignments.(precolored_virtual_idx) <- 0;

        (* color_ifg will call check_coloring *)
        let assignments = 
            match color_ifg ifg num_physical num_virtuals assignments with
            | Allocation assignments -> assignments
            | Spills _ -> failwith "Should not have had to spill"
        in

        (* Check precoloring *)
        assert (assignments.(precolored_virtual_idx) = 0);

        let min_phys_reg = Array.fold_left min Int.max_int assignments in
        let max_phys_reg = Array.fold_left max Int.min_int assignments in

        (* The algorithm picks the lowest available, so there should always be
        register 0 used somewhere. *)
        assert (min_phys_reg = 0);

        (* Max register used should be in range *)
        assert (max_phys_reg < num_physical);
    done;
end

let test_find_live_out () = begin
    let num_virtuals = 100 in
    let num_runs = 10 in
    let num_tests = 10 in
    for i = 1 to num_runs do
        let insts = random_insts num_virtuals in
        let num_insts = List.length insts in
        let successors = List.map (fun {successors} -> successors) insts |> Array.of_list in
        let bit_defs, bit_uses = find_bit_defs_and_uses insts num_insts num_virtuals in
        let bit_live_out = find_live_out bit_defs bit_uses successors num_insts num_virtuals in
        for j = 1 to num_tests do
            check_live_out bit_defs bit_uses successors bit_live_out (Random.int num_insts);
        done;
    done;
end

let tests () = begin
    test_random_ifg ();
    test_find_live_out ();
end