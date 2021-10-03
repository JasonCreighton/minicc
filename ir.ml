type local_id = int
type label_id = int

type datatype =
    | UInt8
    | Int8
    | UInt16
    | Int16
    | UInt32
    | Int32
    | UInt64
    | Int64

type binop =
    | Add
    | Mul
    | Div
    | DivUnsigned
    | Rem
    | RemUnsigned
    | And
    | Or
    | Xor
    | ShiftLeft
    | ShiftRight
    | ShiftRightUnsigned
    | CompEQ
    | CompLT
    | CompLTUnsigned

type unaryop =
    | Neg
    | Not

type expr =
    | BinOp of binop * expr * expr
    | UnaryOp of unaryop * expr
    | ConstInt of int64
    | ConstStringAddr of string
    | Load of datatype * expr
    | LocalAddr of local_id

type inst =
    | Call of local_id * string * expr list
    | Store of datatype * expr * expr
    | Label of label_id
    | Jump of label_id
    | JumpIf of label_id * expr
    | Return of expr

type local_def = {
    size : int;
    alignment : int;
}

type func = {    
    insts : inst list;
    locals : (local_id * local_def) list;
}

let eval_unaryop op x =
    match op with
    | Neg -> Int64.neg x
    | Not -> Int64.lognot x

let eval_binop op x y =
    match op with
    | Add -> Int64.add x y
    | Mul -> Int64.mul x y
    | Div -> Int64.div x y
    | DivUnsigned -> Int64.unsigned_div x y
    | Rem -> Int64.rem x y
    | RemUnsigned -> Int64.unsigned_rem x y
    | And -> Int64.logand x y
    | Or -> Int64.logor x y
    | Xor -> Int64.logxor x y
    | ShiftLeft -> Int64.shift_left x (Int64.to_int y)
    | ShiftRight -> Int64.shift_right x (Int64.to_int y)
    | ShiftRightUnsigned -> Int64.shift_right_logical x (Int64.to_int y)
    | CompEQ -> if (Int64.compare x y) = 0 then 1L else 0L
    | CompLT -> if (Int64.compare x y) < 0 then 1L else 0L
    | CompLTUnsigned -> if (Int64.unsigned_compare x y) < 0 then 1L else 0L

let rec normalize e =
    match e with
    | ConstInt _ | ConstStringAddr _ | LocalAddr _ -> e (* Already normalized *)
    | UnaryOp (op, nonnormal_expr) -> begin
        let e1 = normalize nonnormal_expr in
        match op, e1 with
        (* Fold constants *)
        | _, ConstInt x -> ConstInt (eval_unaryop op x)
        (* Remove repeated negations or inversions *)
        | Neg, UnaryOp (Neg, e2) -> e2
        | Not, UnaryOp (Not, e2) -> e2
        | _, _ -> e1
    end
    | BinOp (op, nonnormal_lhs, nonnormal_rhs) -> begin
        let lhs = normalize nonnormal_lhs in
        let rhs = normalize nonnormal_rhs in
        match op, lhs, rhs with
        (* Fold constants *)
        | _, ConstInt x, ConstInt y -> ConstInt (eval_binop op x y)
        (* Put constants on RHS of communtative operators *)
        | (Add | Mul | And | Or | Xor | CompEQ), ConstInt _, _ -> BinOp (op, rhs, lhs)
        | _, _, _ -> BinOp (op, lhs, rhs)
    end
    | Load (typ, nonnormal_expr) -> Load (typ, normalize nonnormal_expr)


let tests () =
    assert ((normalize (BinOp (Add, ConstInt 2L, ConstInt 2L))) = ConstInt 4L);
    assert ((normalize (BinOp (Mul, ConstInt 5L, ConstInt 6L))) = ConstInt 30L);
    assert ((normalize (BinOp (And, ConstInt 10L, LocalAddr 0))) = (BinOp (And, LocalAddr 0, ConstInt 10L)));
    assert ((normalize (UnaryOp (Neg, UnaryOp (Neg, LocalAddr 0)))) = LocalAddr 0);
    assert ((normalize (UnaryOp (Not, UnaryOp (Not, LocalAddr 0)))) = LocalAddr 0);
    assert ((normalize (UnaryOp (Neg, ConstInt 3L))) = ConstInt (-3L));
    assert ((normalize (BinOp (Add, (BinOp (Add, UnaryOp (Neg, ConstInt 2L), ConstInt 5L)), ConstInt 10L))) = ConstInt 13L);