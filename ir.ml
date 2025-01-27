type local_id = int
type label_id = int

exception Type_error of string

(* Every node in an expression has a datatype, either explicitly specified or
implicited determined by its children. BinOps and UnaryOps yield a result of
the same type of their operands, and the types of BinOp operands must match
each other. *)
type datatype =
    | Ptr
    | U8
    | I8
    | U16
    | I16
    | U32
    | I32
    | U64
    | I64
    | F32
    | F64

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
    | ConstFloat of datatype * float
    | ConstStringAddr of string
    | Load of datatype * expr
    | LocalAddr of local_id
    | GlobalAddr of string

type inst =
    | Store of datatype * expr * expr
    | Call of (datatype * local_id) option * string * expr list
    | Label of label_id
    | Jump of label_id
    | JumpIf of label_id * expr
    | Return of expr option

type storage_spec = {
    size : int;
    alignment : int;
}

type func = {    
    insts : inst list;
    locals : (local_id * storage_spec) list;
}

type compilation_unit = {
    extern_symbols : string list;
    global_variables : (string * storage_spec) list;
    func_table : (string, func) Hashtbl.t;
}

let int64_of_bool b = if b then 1L else 0L

let rec typecheck_expr expr =
    match expr with
    | BinOp (op, lhs, rhs) -> begin
        let lhs_t = typecheck_expr lhs in
        let rhs_t = typecheck_expr rhs in
        if lhs_t <> rhs_t then raise (Type_error "BinOp operands must have matching type");

        (match lhs_t, op with
        | (F32 | F64), (Rem | And | Or | Xor | ShiftLeft | ShiftRight) -> raise (Type_error "Bitwise operators not supported on floating point numbers")
        | _, _ -> ()
        );

        match op with
        | CompEQ | CompNEQ | CompLT | CompLTE | CompGT | CompGTE -> I32 (* Compare operators always yield I32 *)
        | _ -> lhs_t (* Most operators yield the type of the operands *)
    end
    | UnaryOp (LogicalNot, e) -> typecheck_expr e |> ignore; I32
    | UnaryOp (_, e) -> typecheck_expr e
    | Load (typ, addr) -> begin
        let addr_t = typecheck_expr addr in
        if addr_t <> Ptr then raise (Type_error "Load address must have type Ptr");
        typ
    end
    | ConvertTo (typ, e) -> typecheck_expr e |> ignore; typ
    | ConstInt (typ, _) | ConstFloat (typ, _) -> typ
    | ConstStringAddr _ | LocalAddr _ | GlobalAddr _ -> Ptr

let typecheck_inst inst =
    match inst with
    | Store (store_t, addr_expr, value_expr) -> begin
        let addr_t = typecheck_expr addr_expr in
        let value_t = typecheck_expr value_expr in
        if addr_t <> Ptr then raise (Type_error "Store address must have type Ptr");
        if value_t <> store_t then raise (Type_error "Store value type must match type of Store");
    end
    | Call (_, _, arguments) -> List.iter (fun e -> typecheck_expr e |> ignore) arguments
    | Return (Some e) -> typecheck_expr e |> ignore
    | Return None -> ()
    | Label _ | Jump _ | JumpIf _ -> ()

let is_signed typ =
    match typ with
    | I8 | I16 | I32 | I64 -> true
    | U8 | U16 | U32 | U64 | Ptr -> false
    | F32 | F64 -> failwith "Unexpected float type in integer context"

let is_integer typ =
    match typ with
    | I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Ptr -> true
    | F32 | F64 -> false

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
    | Ptr -> zero_extend 64 x
    | F32 | F64 -> failwith "Unexpected float type in integer context"

let eval_unaryop typ op x =
    let full_width_result = match op with
    | Neg -> Int64.neg x
    | Not -> Int64.lognot x
    | LogicalNot -> int64_of_bool (x = 0L)
    in
    limit_width typ full_width_result

let eval_binop typ op x y =
    let signed = is_signed typ in
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

let convert to_type from_type expr =
    assert ((typecheck_expr expr) = from_type);
    if to_type <> from_type
    then ConvertTo (to_type, expr)
    else expr

let rec normalize e =
    match e with
    | ConstInt _ | ConstFloat _ | ConstStringAddr _ | LocalAddr _ | GlobalAddr _ -> e (* Already normalized *)
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
    | ConvertTo (typ, nonnormal_expr) -> ConvertTo (typ, normalize nonnormal_expr)

let assert_raises_type_error f =
    try
        f ();
        failwith "Expected type error"
    with Type_error _ -> ()

let test_normalize () = begin
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
end

let test_typecheck_expr () = begin
    assert_raises_type_error (fun () -> typecheck_expr (BinOp (Add, ConstInt (I32, 0L), ConstInt (I64, 1L))));
    assert_raises_type_error (fun () -> typecheck_expr (BinOp (Add, ConstFloat (F32, 2.0), ConstInt (I64, 1L))));
    assert_raises_type_error (fun () -> typecheck_expr (BinOp (Xor, ConstFloat (F64, 2.0), ConstFloat (F64, 5.0))));
end

let tests () = begin
    test_normalize ();
    test_typecheck_expr ();
end