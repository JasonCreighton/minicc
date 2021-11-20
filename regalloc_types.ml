type virt_reg = int
type phys_reg = int

type operation =
    | Move of virt_reg * virt_reg
    | Inst of { defs: virt_reg list; uses: virt_reg list }

type inst = {
    op: operation;
    successors: int list;
    execution_estimate: int;
}