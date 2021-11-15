type t = {
    num_virtuals: int;
    ifg: (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t;
    assignments: int array;
}

let create num_virtuals =
    let ifg = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout num_virtuals num_virtuals in
    Bigarray.Array2.fill ifg 0;
    let assignments = Array.make num_virtuals (-1) in
    { num_virtuals; ifg; assignments; }

let interferes_with ra i j = begin
    ra.ifg.{i, j} <- 1;
    ra.ifg.{j, i} <- 1;
end

let precolor ra virt_reg phys_reg = Array.set ra.assignments virt_reg phys_reg

let check_coloring ra = begin
    for i = 0 to ra.num_virtuals - 1 do
        for j = i + 1 to ra.num_virtuals - 1 do
            (* If two virtuals interfere, they must not share a physical registers *)
            if ra.ifg.{i, j} = 1 then begin
                assert (ra.assignments.(i) <> ra.assignments.(j));
            end;
        done;
    done;
end

let allocate ra = begin
    let available_phys_regs = Array.make ra.num_virtuals true in
    for virt_reg = 0 to ra.num_virtuals - 1 do
        (* If virt_reg is not yet assigned *)
        if ra.assignments.(virt_reg) = (-1) then begin
            (* Reset set of available registers *)
            Array.fill available_phys_regs 0 ra.num_virtuals true;

            (* Find conflicting virtuals that have already been assigned and
            remove their physical registers from the set of avaialble
            registers. *)
            for other_reg = 0 to ra.num_virtuals - 1 do
                let other_assignment = ra.assignments.(other_reg) in

                if other_assignment <> (-1) && ra.ifg.{virt_reg, other_reg} = 1 then begin
                    available_phys_regs.(ra.assignments.(other_reg)) <- false;
                end;
            done;

            (* Assign lowest-numbered available physical register *)
            let phys_idx = ref 0 in
            let quit = ref false in
            while not !quit do
                if available_phys_regs.(!phys_idx) then
                    quit := true
                else
                    phys_idx := !phys_idx + 1
            done;

            (* Assign register *)
            ra.assignments.(virt_reg) <- !phys_idx;
        end;
    done;

    check_coloring ra;

    ra.assignments
end

let random_ra num_virtuals num_edges = begin
    let ra = create num_virtuals in
    for i = 1 to num_edges do
        interferes_with ra (Random.int num_virtuals) (Random.int num_virtuals)
    done;
    ra
end

let test_random () = begin
    let num_virtuals = 50 in
    let num_edges = 1000 in
    let num_runs = 10 in

    for i = 1 to num_runs do
        let ra = random_ra num_virtuals num_edges in

        (* Precolor one virtual register as physical register 0, which should
        never hurt colorability. *)
        let precolored_virtual_idx = Random.int num_virtuals in
        precolor ra precolored_virtual_idx 0;

        (* allocate will call check_coloring *)
        let assignments = allocate ra in

        (* Check precoloring *)
        assert (assignments.(precolored_virtual_idx) = 0);

        let min_phys_reg = Array.fold_left min Int.max_int assignments in
        let max_phys_reg = Array.fold_left max Int.min_int assignments in

        (* The algorithm picks the lowest available, so there should always be
        register 0 used somewhere. *)
        assert (min_phys_reg = 0);

        (* Unless the graph is total, I think there should always be at least
        one virtual that can share another physical registers. *)
        assert (max_phys_reg < (num_virtuals - 1));
    done;
end

let tests () = begin
    test_random ();
end