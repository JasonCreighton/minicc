type local_id = int
type label_id = int

exception Type_error of string

(* Every node in an expression has a datatype, either explicitly specified or
implicited determined by its children. BinOps and UnaryOps yield a result of
the same type of their operands, and the types of BinOp operands must match
each other. *)
type datatype =
    | U8
    | I8
    | U16
    | I16
    | U32
    | I32
    | U64
    | I64

type binop =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | And
    | Or
    | Xor
    | ShiftLeft
    | ShiftRight
    | CompEQ
    | CompNEQ
    | CompLT
    | CompLTE
    | CompGT
    | CompGTE

type unaryop =
    | Neg
    | Not
    | LogicalNot

type expr =
    | BinOp of binop * expr * expr
    | UnaryOp of unaryop * expr
    | ConvertTo of datatype * expr
    | ConstInt of datatype * int64
    | ConstStringAddr of string
    | Load of datatype * expr
    | LocalAddr of local_id

type inst =
    | Store of datatype * expr * expr
    | Call of (datatype * local_id) option * string * expr list
    | Label of label_id
    | Jump of label_id
    | JumpIf of label_id * expr
    | Return of expr option

type local_def = {
    size : int;
    alignment : int;
}

type func = {    
    insts : inst list;
    locals : (local_id * local_def) list;
}

let rec type_of expr =
    match expr with
    | BinOp (_, lhs, rhs) -> begin
        let lhs_t = type_of lhs in
        assert (lhs_t = (type_of rhs));
        lhs_t
    end
    | UnaryOp (_, e) -> type_of e
    | ConstInt (typ, _) | Load (typ, _) | ConvertTo (typ, _) -> typ
    | ConstStringAddr _ | LocalAddr _ -> U64

let size_of typ =
    match typ with
    | U8  | I8  -> 8
    | U16 | I16 -> 16
    | U32 | I32 -> 32
    | U64 | I64 -> 64

let logical_not x = UnaryOp (LogicalNot, x)

let zero_extend size x =
    let mask = Int64.sub (Int64.shift_left 1L size) 1L in
    Int64.logand x mask

let sign_extend size x =
    let shift_amount = 64 - size in

    (* Put "our" sign bit in actual MSB, then shift right to replicate it in
    the bits above "size" *)
    Int64.shift_right (Int64.shift_left x shift_amount) shift_amount

let limit_width typ x =
    match typ with
    | U8  -> zero_extend 8  x
    | U16 -> zero_extend 16 x
    | U32 -> zero_extend 32 x
    | U64 -> zero_extend 64 x
    | I8  -> sign_extend 8  x
    | I16 -> sign_extend 16 x
    | I32 -> sign_extend 32 x
    | I64 -> sign_extend 64 x

let eval_unaryop typ op x =
    let full_width_result = match op with
    | Neg -> Int64.neg x
    | Not -> Int64.lognot x
    in
    limit_width typ full_width_result

let int64_of_bool b = if b then 1L else 0L

let eval_binop typ op x y =
    let signed =
        match typ with
        | I8 | I16 | I32 | I64 -> true
        | U8 | U16 | U32 | U64 -> false
    in
    let cmp = if signed then Int64.compare x y else Int64.unsigned_compare x y in
    let full_width_result = match op with
    | Add -> Int64.add x y
    | Sub -> Int64.sub x y
    | Mul -> Int64.mul x y
    | Div -> if signed then Int64.div x y else Int64.unsigned_div x y
    | Rem -> if signed then Int64.rem x y else Int64.unsigned_rem x y
    | And -> Int64.logand x y
    | Or -> Int64.logor x y
    | Xor -> Int64.logxor x y
    | ShiftLeft -> Int64.shift_left x (Int64.to_int y)
    | ShiftRight ->
        if signed
        then Int64.shift_right x (Int64.to_int y)
        else Int64.shift_right_logical x (Int64.to_int y)
    | CompEQ -> int64_of_bool (cmp = 0)
    | CompNEQ -> int64_of_bool (cmp <> 0)
    | CompLT -> int64_of_bool (cmp < 0)
    | CompLTE -> int64_of_bool (cmp <= 0)
    | CompGT -> int64_of_bool (cmp > 0)
    | CompGTE -> int64_of_bool (cmp >= 0)
    in
    limit_width typ full_width_result

let rec normalize e =
    match e with
    | ConstInt _ | ConstStringAddr _ | LocalAddr _ -> e (* Already normalized *)
    | UnaryOp (op, nonnormal_expr) -> begin
        let e1 = normalize nonnormal_expr in
        match op, e1 with
        (* Fold constants *)
        | _, ConstInt (typ, x) -> ConstInt (typ, eval_unaryop typ op x)
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
        | _, ConstInt (typ_x, x), ConstInt (typ_y, y) -> begin
            if typ_x <> typ_y then raise (Type_error "BinOp operands have different types");
            ConstInt (typ_x, eval_binop typ_x op x y)
        end
        (* Put constants on RHS of communtative operators *)
        | (Add | Mul | And | Or | Xor | CompEQ | CompNEQ), ConstInt _, _ -> BinOp (op, rhs, lhs)
        | _, _, _ -> BinOp (op, lhs, rhs)
    end
    | Load (typ, nonnormal_expr) -> Load (typ, normalize nonnormal_expr)


let tests () =
    assert ((normalize (BinOp (Add, ConstInt (I64, 2L), ConstInt (I64, 2L)))) = ConstInt (I64, 4L));
    assert ((normalize (BinOp (Mul, ConstInt (I64, 5L), ConstInt (I64, 6L)))) = ConstInt (I64, 30L));
    assert ((normalize (BinOp (And, ConstInt (I64, 10L), LocalAddr 0))) = (BinOp (And, LocalAddr 0, ConstInt (I64, 10L))));
    assert ((normalize (UnaryOp (Neg, UnaryOp (Neg, LocalAddr 0)))) = LocalAddr 0);
    assert ((normalize (UnaryOp (Not, UnaryOp (Not, LocalAddr 0)))) = LocalAddr 0);
    assert ((normalize (UnaryOp (Neg, ConstInt (I64, 3L)))) = ConstInt (I64, (-3L)));
    assert ((normalize (BinOp (Add, (BinOp (Add, UnaryOp (Neg, ConstInt (I64, 2L)), ConstInt (I64, 5L))), ConstInt (I64, 10L)))) = ConstInt (I64, 13L));

    (* Wrapping of less than full size integers *)
    assert ((normalize (BinOp (Add, ConstInt (I8, 127L), ConstInt (I8, 1L)))) = ConstInt (I8, -128L));
    assert ((normalize (BinOp (Add, ConstInt (U8, 255L), ConstInt (U8, 1L)))) = ConstInt (U8, 0L));
    assert ((normalize (BinOp (Add, ConstInt (U16, 65535L), ConstInt (U16, 1L)))) = ConstInt (U16, 0L));
    assert ((normalize (BinOp (Add, ConstInt (I32, -2147483648L), ConstInt (I32, -1L)))) = ConstInt (I32, 2147483647L));