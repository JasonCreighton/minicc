type virt_reg = int
type phys_reg = int

type reg_class = {
    from_idx: phys_reg;
    to_idx: phys_reg;
}

type allocation_result =
    | Spills of virt_reg list
    | Allocation of phys_reg array

type operation =
    | Move of virt_reg * virt_reg
    | Inst of { defs: virt_reg list; uses: virt_reg list }

type inst = {
    op: operation;
    successors: int list;
    execution_estimate: int;
}