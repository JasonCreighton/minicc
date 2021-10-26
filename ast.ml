open Printf

exception Compile_error of string

module IntLitFlags = struct
    type t = int
    let decimal = 0x1
    let unsigned = 0x2
    let long = 0x4
end

type int_size =
    | Char
    | Short
    | Int
    | Long

type ctype =
    | Void
    | Signed of int_size
    | Unsigned of int_size
    | Float
    | Double
    | PointerTo of ctype
    | ArrayOf of ctype * int

type binop =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | BitAnd
    | BitXor
    | BitOr
    | BitShiftLeft
    | BitShiftRight
    | CompEQ
    | CompNEQ
    | CompLT
    | CompGT
    | CompLTE
    | CompGTE

type unaryop =
    | PreInc
    | PreDec
    | PostInc
    | PostDec
    | LogicalNot
    | BitNot
    | Neg
    | AddressOf

type decl =
    | Function of ctype * string * (ctype * string) list * stmt
    | FunctionDecl of ctype * string * (ctype * string) list
and stmt =
    | CompoundStmt of stmt list
    | ExprStmt of expr
    | IfElseStmt of expr * stmt * stmt
    | DeclVar of ctype * string
    | DeclAssign of ctype * string * expr
    | WhileStmt of expr * stmt
    | ForStmt of expr * expr * expr * stmt
    | ReturnStmt of expr option
and expr =
    | Lit of int64 * IntLitFlags.t
    | LitString of string
    | Assign of expr * expr
    | VarRef of string
    | Subscript of expr * expr
    | Deref of expr
    | BinOp of binop * expr * expr
    | UnaryOp of unaryop * expr
    | Conditional of expr * expr * expr (* "ternary" operator *)
    | Sequence of expr * expr (* "comma" operator *)
    (* Technically logical AND/OR are binary operators, but they are special because
     of short circuiting and will have different code gen, so we separate them *)
    | LogicalAnd of expr * expr
    | LogicalOr of expr * expr 
    | Call of string * expr list

let sizeof_int int_size =
    match int_size with
    | Char -> 1
    | Short -> 2
    | Int -> 4
    | Long -> 8

let rec sizeof ctype =
    match ctype with
    | Void -> raise (Compile_error "Can't use void in sized context")
    | Signed x | Unsigned x -> sizeof_int x
    | Float -> 4
    | Double -> 8
    | PointerTo _ -> 8
    | ArrayOf (ctype, size) -> (sizeof ctype) * size

let ir_datatype_for_ctype ctype =
    match ctype with
    | Signed Char -> Ir.I8
    | Unsigned Char -> Ir.U8
    | Signed Short -> Ir.I16
    | Unsigned Short -> Ir.U16
    | Signed Int -> Ir.I32
    | Unsigned Int -> Ir.U32
    | Signed Long -> Ir.I64
    | Unsigned Long -> Ir.U64
    | PointerTo _ -> Ir.Ptr

let usual_arithmetic_conversions lhs_ctype rhs_ctype =
    match lhs_ctype, rhs_ctype with
    | _ when lhs_ctype = rhs_ctype -> lhs_ctype (* No conversion necessary *)
    | (Signed lhs_size as lhs_t), (Signed rhs_size as rhs_t) | (Unsigned lhs_size as lhs_t), (Unsigned rhs_size as rhs_t) ->
        (* If they are of the same signedness, then promote to the largest size *)
        if sizeof_int lhs_size > sizeof_int rhs_size
        then lhs_t
        else rhs_t
    | Signed signed_size, Unsigned unsigned_size | Unsigned unsigned_size, Signed signed_size ->
        (* If they are of different signedness and the unsigned operand is the
        same same or larger than the signed operand, convert to unsigned.
        Otherwise, convert to signed. *)
        if sizeof_int unsigned_size >= sizeof_int signed_size
        then Unsigned unsigned_size
        else Signed signed_size

let ctype_for_integer_literal n (flags : IntLitFlags.t) =
    let fits_int = Int64.unsigned_compare n (Int64.shift_left 1L 31) < 0 in
    let fits_uint = Int64.unsigned_compare n (Int64.shift_left 1L 32) < 0 in
    let fits_long = Int64.unsigned_compare n (Int64.shift_left 1L 63) < 0 in
    let fits_ulong = true in
    let signed_okay = (flags land IntLitFlags.unsigned) = 0 in
    let unsigned_okay = ((flags land IntLitFlags.unsigned) <> 0) || ((flags land IntLitFlags.decimal) = 0) in
    let int_okay = (flags land IntLitFlags.long) = 0 in
    let long_okay = true in

    (* Find the smallest type that fits the literal, subject to the
    restrictions implied by the flags. *)
    if      fits_int   && int_okay  && signed_okay   then Signed   Int
    else if fits_uint  && int_okay  && unsigned_okay then Unsigned Int
    else if fits_long  && long_okay && signed_okay   then Signed   Long
    else if fits_ulong && long_okay && unsigned_okay then Unsigned Long
    else failwith "Could not find type for integer literal"

let func_to_ir _ _ func_params func_body =
    let locals = ref [] in
    let insts = ref [] in
    let var_table = Hashtbl.create 100 in
    let next_label_id = ref 0 in
    let next_local_id = ref 0 in

    (* Helper functions *)
    let add_inst i = Ir.typecheck_inst i; insts := i :: !insts in
    let new_label () = let l = !next_label_id in next_label_id := !next_label_id + 1; l in
    let new_local ctype =
        let local_id = !next_local_id in
        let size = sizeof ctype in
        locals := (local_id, {Ir.size=size; Ir.alignment=size}) :: !locals;
        next_local_id := !next_local_id + 1;
        local_id
    in
    let decl_var ctype v =
        (* Record in variable table *)
        Hashtbl.add var_table v (new_local ctype, ctype)
    in
    let find_var_loc v =
        match Hashtbl.find_opt var_table v with
        | Some loc -> loc
        | None -> raise (Compile_error (sprintf "Undeclared variable '%s'" v))
    in

    (* Mutually recursive functions to walk AST *)
    let rec address_of_lvalue expr =
        match expr with
        | VarRef v -> let local_id, ctype = find_var_loc v in (ctype, Ir.LocalAddr local_id)
        | Subscript (ary, idx) -> begin
            let (ArrayOf (elem_ctype, _), addr_ir) = address_of_lvalue ary in
            let _, idx_ir = emit_expr idx in
            (elem_ctype, Ir.BinOp (Ir.Add, addr_ir, Ir.BinOp (Ir.Mul, Ir.ConvertTo (Ir.Ptr, idx_ir), Ir.ConstInt (Ir.Ptr, Int64.of_int (sizeof elem_ctype)))))
        end
        | Deref addr_expr ->
            let (PointerTo ctype, addr_ir) = address_of_lvalue addr_expr in
            (ctype, Ir.Load (Ir.Ptr, addr_ir))
        | _ -> raise (Compile_error "expr is not an lvalue")
    and assign_var v expr =
        let local_id, ctype = find_var_loc v in
        let expr_ctype, expr_ir = emit_expr expr in
        let ir_datatype = ir_datatype_for_ctype ctype in
        let converted_ir = if expr_ctype <> ctype then Ir.ConvertTo (ir_datatype, expr_ir) else expr_ir in
        add_inst @@ Ir.Store (ir_datatype, Ir.LocalAddr local_id, converted_ir);
        (ctype, Ir.Load (ir_datatype, Ir.LocalAddr local_id))
    and inc_or_dec yield_old_value delta expr =
        match expr with
        | VarRef v -> begin
            let local_id, ctype = find_var_loc v in
            let ir_datatype = ir_datatype_for_ctype ctype in

            (* Always pre-increment *)
            add_inst @@ Ir.Store (ir_datatype, Ir.LocalAddr local_id, Ir.BinOp (Ir.Add, Ir.Load (ir_datatype, Ir.LocalAddr local_id), Ir.ConstInt (ir_datatype, Int64.of_int delta)));

            (* FIXME: For post-increment, rather than doing the increment at
            the appropriate time, we just add/subtract the appropriate amount
            to recover the original value. *)
            let adj = if yield_old_value then -delta else 0 in
            (ctype, Ir.BinOp (Ir.Add, (Ir.Load (ir_datatype, Ir.LocalAddr local_id)), Ir.ConstInt (ir_datatype, Int64.of_int adj)))
        end
        | Subscript (_, _) -> failwith "TODO: Implement array inc/dec"
        (* TODO: This is a ugly way to clear the fragile pattern match warning *)
        |Lit _|LitString _|Assign (_, _)|BinOp (_, _, _)|UnaryOp (_, _)|Conditional (_, _, _)|Sequence (_, _)|LogicalAnd (_, _)|LogicalOr (_, _)|Call (_, _) -> raise (Compile_error "Pre-Increment/Decrement of non-lvalue")
    and emit_loop init_opt cond incr_opt body = begin
        let test_label_id = new_label () in
        let end_label_id = new_label () in

        Option.iter (fun e -> emit_expr e |> ignore) init_opt;

        (* Loop test *)
        add_inst @@ Ir.Label test_label_id;
        let _, cond_ir = emit_expr cond in
        add_inst @@ Ir.JumpIf (end_label_id, Ir.logical_not cond_ir);

        (* Loop body *)
        emit_stmt body;

        (* Loop increment *)
        Option.iter (fun e -> emit_expr e |> ignore) incr_opt;

        add_inst @@ Ir.Jump test_label_id;
        add_inst @@ Ir.Label end_label_id
    end
    and emit_short_circuit_logical_op short_circuit_condition e1 e2 = begin
        let result_local_id = new_local (Unsigned Char) in
        let short_circuit_label_id = new_label () in
        let e1_ctype, e1_ir = emit_expr e1 in
        add_inst @@ Ir.Store (Ir.U8, Ir.LocalAddr result_local_id, Ir.logical_not (Ir.BinOp (Ir.CompEQ, e1_ir, Ir.ConstInt (ir_datatype_for_ctype e1_ctype, 0L))));
        add_inst @@ Ir.JumpIf (short_circuit_label_id, Ir.BinOp (Ir.CompEQ, Ir.Load(Ir.U8, Ir.LocalAddr result_local_id), Ir.ConstInt (Ir.U8, if short_circuit_condition then 1L else 0L)));
        let e2_ctype, e2_ir = emit_expr e2 in
        add_inst @@ Ir.Store (Ir.U8, Ir.LocalAddr result_local_id, Ir.logical_not (Ir.BinOp (Ir.CompEQ, e2_ir, Ir.ConstInt (ir_datatype_for_ctype e2_ctype, 0L))));
        add_inst @@ Ir.Label short_circuit_label_id;

        (Unsigned Char, Ir.Load (Ir.U8, Ir.LocalAddr result_local_id))
    end
    and emit_stmt stmt =
        match stmt with
        | DeclVar (ctype, v) -> decl_var ctype v
        | DeclAssign (ctype, v, expr) -> decl_var ctype v; assign_var v expr |> ignore
        | ExprStmt expr -> emit_expr expr |> ignore
        | CompoundStmt stmts -> List.iter emit_stmt stmts
        | IfElseStmt (cond, then_stmt, else_stmt) -> begin
            let else_label_id = new_label () in
            let done_label_id = new_label () in
            let _, cond_ir = emit_expr cond in
            add_inst @@ Ir.JumpIf (else_label_id, Ir.logical_not cond_ir);
            emit_stmt then_stmt;
            add_inst @@ Ir.Jump done_label_id;
            add_inst @@ Ir.Label else_label_id;
            emit_stmt else_stmt;
            add_inst @@ Ir.Label done_label_id;
        end
        | WhileStmt (cond, body) -> emit_loop Option.none cond Option.none body
        | ForStmt (init, cond, incr, body) -> emit_loop (Option.some init) cond (Option.some incr) body
        | ReturnStmt expr_opt ->
            add_inst @@ Ir.Return (Option.map (fun e -> emit_expr e |> snd) expr_opt)
    and emit_expr expr =
        match expr with
        | Lit (n, flags) ->
            let ctype = ctype_for_integer_literal n flags in
            (ctype, Ir.ConstInt(ir_datatype_for_ctype ctype, n))
        | LitString s -> (PointerTo (Unsigned Char), Ir.ConstStringAddr s)
        | Assign (VarRef v, rhs) -> assign_var v rhs
        | Assign (lvalue, rhs) -> begin
            let lhs_ctype, lhs_addr_ir = address_of_lvalue lvalue in
            let _, rhs_ir = emit_expr rhs in
            add_inst @@ Ir.Store (ir_datatype_for_ctype lhs_ctype, lhs_addr_ir, rhs_ir);

            (lhs_ctype, Ir.Load (ir_datatype_for_ctype lhs_ctype, lhs_addr_ir))
        end
        | VarRef v -> let local_id, ctype = find_var_loc v in (ctype, Ir.Load (ir_datatype_for_ctype ctype, Ir.LocalAddr local_id))
        | Subscript (_, _) -> begin
            let elem_ctype, addr_ir = address_of_lvalue expr in
            (elem_ctype, Ir.Load (ir_datatype_for_ctype elem_ctype, addr_ir))
        end
        | Deref e -> let (PointerTo ctype, addr_ir) = emit_expr e in (ctype, Ir.Load (ir_datatype_for_ctype ctype, addr_ir))
        | BinOp (op, e1, e2) -> begin
            let e1_ctype, e1_ir = emit_expr e1 in
            let e2_ctype, e2_ir = emit_expr e2 in
            let result_ctype = usual_arithmetic_conversions e1_ctype e2_ctype in
            let result_irtype = ir_datatype_for_ctype result_ctype in
            let e1_ir = if e1_ctype <> result_ctype then Ir.ConvertTo (result_irtype, e1_ir) else e1_ir in
            let e2_ir = if e2_ctype <> result_ctype then Ir.ConvertTo (result_irtype, e2_ir) else e2_ir in

            let result_ir = match op with
            | Add -> Ir.BinOp (Ir.Add, e1_ir, e2_ir)
            | Sub -> Ir.BinOp (Ir.Sub, e1_ir, e2_ir)
            | Mul -> Ir.BinOp (Ir.Mul, e1_ir, e2_ir)
            | Div -> Ir.BinOp (Ir.Div, e1_ir, e2_ir)
            | Rem -> Ir.BinOp (Ir.Rem, e1_ir, e2_ir)
            | BitAnd -> Ir.BinOp (Ir.And, e1_ir, e2_ir)
            | BitOr -> Ir.BinOp (Ir.Or, e1_ir, e2_ir)
            | BitXor -> Ir.BinOp (Ir.Xor, e1_ir, e2_ir)
            | BitShiftLeft -> Ir.BinOp (Ir.ShiftLeft, e1_ir, e2_ir)
            | BitShiftRight -> Ir.BinOp (Ir.ShiftRight, e1_ir, e2_ir)
            | CompEQ -> Ir.BinOp (Ir.CompEQ, e1_ir, e2_ir)
            | CompNEQ -> Ir.BinOp (Ir.CompNEQ, e1_ir, e2_ir)
            | CompLT -> Ir.BinOp (Ir.CompLT, e1_ir, e2_ir)
            | CompGT -> Ir.BinOp (Ir.CompGT, e1_ir, e2_ir)
            | CompLTE -> Ir.BinOp (Ir.CompLTE, e1_ir, e2_ir)
            | CompGTE -> Ir.BinOp (Ir.CompGTE, e1_ir, e2_ir)
            in

            (result_ctype, result_ir)
        end
        | UnaryOp (op, e) -> begin
            match op with
            | PreInc -> inc_or_dec false 1 e
            | PreDec -> inc_or_dec false (-1) e
            | PostInc -> inc_or_dec true 1 e
            | PostDec -> inc_or_dec true (-1) e
            | BitNot -> let ctype, e_ir = emit_expr e in (ctype, Ir.UnaryOp (Ir.Not, e_ir))
            | LogicalNot -> let ctype, e_ir = emit_expr e in (ctype, Ir.logical_not e_ir)
            | Neg -> let ctype, e_ir = emit_expr e in (ctype, Ir.UnaryOp (Ir.Neg, e_ir))
            | AddressOf -> let ctype, e_ir = address_of_lvalue e in (PointerTo ctype, e_ir)
        end
        | Conditional (cond, true_expr, false_expr) -> begin
            let else_label_id = new_label () in
            let done_label_id = new_label () in
            let _, cond_ir = emit_expr cond in
            add_inst @@ Ir.JumpIf (else_label_id, Ir.logical_not cond_ir);
            let true_ctype, true_ir = emit_expr true_expr in
            let result_local_id = new_local true_ctype in
            add_inst @@ Ir.Store (ir_datatype_for_ctype true_ctype, Ir.LocalAddr result_local_id, true_ir);
            add_inst @@ Ir.Jump done_label_id;
            add_inst @@ Ir.Label else_label_id;
            let false_ctype, false_ir = emit_expr false_expr in
            add_inst @@ Ir.Store (ir_datatype_for_ctype false_ctype, Ir.LocalAddr result_local_id, false_ir);
            add_inst @@ Ir.Label done_label_id;
            (true_ctype, Ir.Load (ir_datatype_for_ctype true_ctype, Ir.LocalAddr result_local_id))
        end
        | Sequence (e1, e2) -> emit_expr e1 |> ignore; emit_expr e2
        | LogicalAnd (e1, e2) -> emit_short_circuit_logical_op false e1 e2
        | LogicalOr (e1, e2) -> emit_short_circuit_logical_op true e1 e2
        | Call(func_name, args) ->
            begin
                let result_local_id = new_local (Signed Long) in (* FIXME: Don't hardcode type *)
                let evaluated_args = List.map (fun e -> emit_expr e |> snd) args in
                add_inst @@ Ir.Call (Some (Ir.U64, result_local_id), func_name, evaluated_args);

                (Signed Long, Ir.Load (Ir.U64, Ir.LocalAddr result_local_id)) (* FIXME: Don't hardcode type *)
            end
    in
    (* Put arguments into var_table *)
    List.iteri (fun i (ctype, arg_name) -> Hashtbl.add var_table arg_name (-(i + 1), ctype)) func_params;

    emit_stmt func_body;
    { Ir.insts = List.rev !insts; Ir.locals = !locals; }

let build_func_table decl_list =
    let func_table = Hashtbl.create 64 in
    List.iter (fun d ->
        match d with
        | Function (ret_ctype, name, params, body) -> Hashtbl.add func_table name (func_to_ir ret_ctype name params body)
        | FunctionDecl _ -> ()
    ) decl_list;

    func_table

let test_ctype_for_integer_literal () = begin
    assert ((ctype_for_integer_literal 0L 0) = Signed Int);
    assert ((ctype_for_integer_literal 2000000000L 0) = Signed Int);
    assert ((ctype_for_integer_literal 2000000000L IntLitFlags.long) = Signed Long);
    assert ((ctype_for_integer_literal 3000000000L 0) = Unsigned Int);
    assert ((ctype_for_integer_literal 3000000000L IntLitFlags.decimal) = Signed Long);
    assert ((ctype_for_integer_literal 3000000000L (IntLitFlags.decimal lor IntLitFlags.unsigned)) = Unsigned Int);
    assert ((ctype_for_integer_literal 5000000000L (IntLitFlags.decimal lor IntLitFlags.unsigned)) = Unsigned Long);
end

let tests () = begin
    test_ctype_for_integer_literal ();
end