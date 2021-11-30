type reg = int

type reg_class = {
    from_idx: reg;
    to_idx: reg;
}

type allocation_result =
    | Spills of reg list
    | Allocation of reg array

type operation =
    | Move of reg * reg
    | Inst of { defs: reg list; uses: reg list }

type inst = {
    op: operation;
    successors: int list;
    execution_estimate: int;
}