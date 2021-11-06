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
    | FunctionDecl of ctype * string * (ctype * string) list * bool
    | GlobalVarDecl of ctype * string
and stmt =
    | CompoundStmt of stmt list
    | ExprStmt of expr
    | IfElseStmt of expr * stmt * stmt
    | DeclVar of ctype * string
    | DeclAssign of ctype * string * expr
    | WhileStmt of expr * stmt
    | ForStmt of expr * expr * expr * stmt
    | LabeledStmt of string * stmt
    | GotoStmt of string
    | BreakStmt
    | ContinueStmt
    | ReturnStmt of expr option
and expr =
    | Lit of int64 * IntLitFlags.t
    | LitDouble of float
    | LitFloat of float
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

type function_prototype = {
    name : string;
    ret_type : ctype;
    arg_types : ctype list;
    is_varargs : bool;
    is_extern : bool;
}

let rec scope_lookup_opt list_of_tbls key =
    match list_of_tbls with
    | [] -> None
    | tbl :: tbls -> begin
        match Hashtbl.find_opt tbl key with
        | None -> scope_lookup_opt tbls key
        | Some _ as found -> found
    end

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

let rec alignment ctype =
    match ctype with
    | Void -> raise (Compile_error "Can't use void in sized context")
    | (Signed _ | Unsigned _ | PointerTo _ | Float | Double) as primtype -> sizeof primtype
    | ArrayOf (elem_ctype, _) -> alignment elem_ctype

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
    | Double -> Ir.F64
    | Float -> Ir.F32
    | Void | ArrayOf _ -> failwith "Can't convert ctype to irtype"

let binop_common_ctype lhs_ctype rhs_ctype =
    match lhs_ctype, rhs_ctype with
    | Double, _ | _, Double -> Double
    | Float, _ | _, Float -> Float
    | _ when sizeof lhs_ctype < 4 && sizeof rhs_ctype < 4 -> Signed Int (* Always promote to at least an int *)
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
    | _, _ -> failwith "Unexpected binop types"

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

let func_to_ir prototype_table global_ctypes ret_ctype _ func_params func_body =
    let locals = ref [] in
    let insts = ref [] in
    let break_labels = ref [] in
    let continue_labels = ref [] in
    let goto_labels = Hashtbl.create 16 in
    let scopes = ref [Hashtbl.create 16] in    
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
        if Hashtbl.mem (List.hd !scopes) v then raise (Compile_error (sprintf "Variable declared twice in same scope: '%s'" v));
        (* Record in variable table *)
        Hashtbl.add (List.hd !scopes) v (new_local ctype, ctype)
    in
    let find_var_addr v =
        (* Try to find a local *)
        match scope_lookup_opt !scopes v with
        | Some (local_id, ctype) -> (ctype, Ir.LocalAddr local_id)
        | None -> begin
            (* Try to find a global *)
            match Hashtbl.find_opt global_ctypes v with
            | Some ctype -> (ctype, Ir.GlobalAddr v)
            | None -> raise (Compile_error (sprintf "Undeclared variable '%s'" v))
        end
    in
    let break_or_continue label_stack = match label_stack with
        | [] -> raise (Compile_error "Can't break/continue outside of loop")
        | label_id :: _ -> add_inst @@ Ir.Jump label_id
    in
    let find_or_create_goto_label label = begin
        if not (Hashtbl.mem goto_labels label) then Hashtbl.add goto_labels label (new_label ());
        
        Hashtbl.find goto_labels label;
    end in
    let begin_scope () = scopes := Hashtbl.create 16 :: !scopes in
    let end_scope () = scopes := List.tl !scopes in

    (* Mutually recursive functions to walk AST *)
    let rec address_of_lvalue expr =
        match expr with
        | VarRef v -> find_var_addr v
        | Subscript (ary, idx) -> begin
            match address_of_lvalue ary with
            | ArrayOf (elem_ctype, _), addr_ir -> begin                
                let _, idx_ir = emit_expr idx in
                (elem_ctype, Ir.BinOp (Ir.Add, addr_ir, Ir.BinOp (Ir.Mul, Ir.ConvertTo (Ir.Ptr, idx_ir), Ir.ConstInt (Ir.Ptr, Int64.of_int (sizeof elem_ctype)))))
            end
            | _ -> failwith "Expected target of subscript to be array type"
        end
        | Deref addr_expr -> begin
            match address_of_lvalue addr_expr with
            | PointerTo ctype, addr_ir -> (ctype, Ir.Load (Ir.Ptr, addr_ir))
            | _ -> failwith "Expected target of deference to be pointer type"
        end
        | _ -> raise (Compile_error "expr is not an lvalue")
    and assign_var v expr =
        let ctype, var_addr_ir = find_var_addr v in
        let expr_ctype, expr_ir = emit_expr expr in
        let ir_datatype = ir_datatype_for_ctype ctype in
        let converted_ir = Ir.convert ir_datatype (ir_datatype_for_ctype expr_ctype) expr_ir in
        add_inst @@ Ir.Store (ir_datatype, var_addr_ir, converted_ir);
        (ctype, Ir.Load (ir_datatype, var_addr_ir))
    and inc_or_dec yield_old_value delta expr = begin
            let ctype, expr_addr_ir = address_of_lvalue expr in
            let ir_datatype = ir_datatype_for_ctype ctype in

            (* Always pre-increment *)
            add_inst @@ Ir.Store (ir_datatype, expr_addr_ir, Ir.BinOp (Ir.Add, Ir.Load (ir_datatype, expr_addr_ir), Ir.ConstInt (ir_datatype, Int64.of_int delta)));

            (* FIXME: For post-increment, rather than doing the increment at
            the appropriate time, we just add/subtract the appropriate amount
            to recover the original value. *)
            let adj = if yield_old_value then -delta else 0 in
            (ctype, Ir.BinOp (Ir.Add, (Ir.Load (ir_datatype, expr_addr_ir)), Ir.ConstInt (ir_datatype, Int64.of_int adj)))
        end
    and eval_arguments is_varargs exprs ctypes =
        match exprs, ctypes with
        | [], [] -> []
        | e :: es, t :: ts -> begin
            let expr_ctype, expr_ir = emit_expr e in
            let expr_irtype = ir_datatype_for_ctype expr_ctype in
            let expected_irtype = ir_datatype_for_ctype t in

            Ir.convert expected_irtype expr_irtype expr_ir :: eval_arguments is_varargs es ts
        end
        | e :: es, [] when is_varargs -> begin
            let expr_ctype, expr_ir = emit_expr e in
            (* varargs floats are always converted to doubles *)
            let expr_ir = match expr_ctype with
            | Float -> Ir.ConvertTo(Ir.F64, expr_ir)
            | _ -> expr_ir
            in
            expr_ir :: eval_arguments is_varargs es []
        end
        | _ :: _, [] -> raise (Compile_error "Too many arguments given")
        | [], _ :: _ -> raise (Compile_error "Not enough arguments given")
    and emit_loop init_opt cond incr_opt body = begin
        let test_label_id = new_label () in
        let body_label_id = new_label () in
        let break_label_id = new_label () in
        let continue_label_id = new_label () in

        (* Push break/continue labels onto stack *)
        break_labels := break_label_id :: !break_labels;
        continue_labels := continue_label_id :: !continue_labels;

        (* Loop init *)
        Option.iter (fun e -> emit_expr e |> ignore) init_opt;

        (* Jump over body of loop to test *)
        add_inst @@ Ir.Jump test_label_id;

        (* Loop body *)
        add_inst @@ Ir.Label body_label_id;
        emit_stmt body;

        (* Loop increment *)
        add_inst @@ Ir.Label continue_label_id;
        Option.iter (fun e -> emit_expr e |> ignore) incr_opt;

        (* Loop test *)
        add_inst @@ Ir.Label test_label_id;
        let _, cond_ir = emit_expr cond in
        add_inst @@ Ir.JumpIf (body_label_id, cond_ir);
        add_inst @@ Ir.Label break_label_id;

        (* Pop break/continue labels off of stack *)
        break_labels := List.tl !break_labels;
        continue_labels := List.tl !continue_labels;
    end
    and emit_short_circuit_logical_op short_circuit_condition e1 e2 = begin
        let result_local_id = new_local (Unsigned Char) in
        let short_circuit_label_id = new_label () in
        let e1_ctype, e1_ir = emit_expr e1 in
        add_inst @@ Ir.Store (Ir.U8, Ir.LocalAddr result_local_id, Ir.ConvertTo (Ir.U8, Ir.logical_not (Ir.BinOp (Ir.CompEQ, e1_ir, Ir.ConstInt (ir_datatype_for_ctype e1_ctype, 0L)))));
        add_inst @@ Ir.JumpIf (short_circuit_label_id, Ir.BinOp (Ir.CompEQ, Ir.Load(Ir.U8, Ir.LocalAddr result_local_id), Ir.ConstInt (Ir.U8, if short_circuit_condition then 1L else 0L)));
        let e2_ctype, e2_ir = emit_expr e2 in
        add_inst @@ Ir.Store (Ir.U8, Ir.LocalAddr result_local_id, Ir.ConvertTo (Ir.U8, Ir.logical_not (Ir.BinOp (Ir.CompEQ, e2_ir, Ir.ConstInt (ir_datatype_for_ctype e2_ctype, 0L)))));
        add_inst @@ Ir.Label short_circuit_label_id;

        (Unsigned Char, Ir.Load (Ir.U8, Ir.LocalAddr result_local_id))
    end
    and emit_stmt stmt =
        match stmt with
        | DeclVar (ctype, v) -> decl_var ctype v
        | DeclAssign (ctype, v, expr) -> decl_var ctype v; assign_var v expr |> ignore
        | ExprStmt expr -> emit_expr expr |> ignore
        | CompoundStmt stmts -> begin
            begin_scope ();
            List.iter emit_stmt stmts;
            end_scope ();
        end
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
        | ForStmt (init, cond, incr, body) -> begin
            begin_scope ();
            emit_loop (Option.some init) cond (Option.some incr) body;
            end_scope ();
        end
        | BreakStmt -> break_or_continue !break_labels
        | ContinueStmt -> break_or_continue !continue_labels
        | GotoStmt label -> add_inst @@ Ir.Jump (find_or_create_goto_label label)
        | LabeledStmt (label, stmt) -> begin
            let label_id = find_or_create_goto_label label in
            add_inst @@ Ir.Label label_id;
            emit_stmt stmt;
        end
        | ReturnStmt expr_opt ->
            add_inst @@ Ir.Return (Option.map (fun e -> begin
                let expr_ctype, expr_ir = emit_expr e in
                let ret_irtype = ir_datatype_for_ctype ret_ctype in
                let expr_irtype = ir_datatype_for_ctype expr_ctype in
                Ir.convert ret_irtype expr_irtype expr_ir
            end
            ) expr_opt)
    and emit_expr expr =
        match expr with
        | Lit (n, flags) ->
            let ctype = ctype_for_integer_literal n flags in
            (ctype, Ir.ConstInt(ir_datatype_for_ctype ctype, n))
        | LitDouble n -> (Double, Ir.ConstFloat (Ir.F64, n))
        | LitFloat n -> (Float, Ir.ConstFloat (Ir.F32, n))
        | LitString s -> (PointerTo (Unsigned Char), Ir.ConstStringAddr s)
        | Assign (VarRef v, rhs) -> assign_var v rhs
        | Assign (lvalue, rhs) -> begin
            let lhs_ctype, lhs_addr_ir = address_of_lvalue lvalue in
            let rhs_ctype, rhs_ir = emit_expr rhs in
            let lhs_irtype = ir_datatype_for_ctype lhs_ctype in
            let rhs_irtype = ir_datatype_for_ctype rhs_ctype in
            add_inst @@ Ir.Store (ir_datatype_for_ctype lhs_ctype, lhs_addr_ir, Ir.convert lhs_irtype rhs_irtype rhs_ir);

            (lhs_ctype, Ir.Load (ir_datatype_for_ctype lhs_ctype, lhs_addr_ir))
        end
        | VarRef v -> let ctype, var_addr_ir = find_var_addr v in (ctype, Ir.Load (ir_datatype_for_ctype ctype, var_addr_ir))
        | Subscript (_, _) -> begin
            let elem_ctype, addr_ir = address_of_lvalue expr in
            (elem_ctype, Ir.Load (ir_datatype_for_ctype elem_ctype, addr_ir))
        end
        | Deref e -> begin
            match emit_expr e with
            | PointerTo ctype, addr_ir -> (ctype, Ir.Load (ir_datatype_for_ctype ctype, addr_ir))
            | _, _ -> failwith "Expected pointer type when dereferencing"
        end
        | BinOp (op, e1, e2) -> begin
            let e1_ctype, e1_ir = emit_expr e1 in
            let e2_ctype, e2_ir = emit_expr e2 in
            let operand_ctype = binop_common_ctype e1_ctype e2_ctype in
            let result_ctype = match op with
                | CompEQ | CompNEQ | CompLT | CompLTE | CompGT | CompGTE -> Signed Int
                | _ -> operand_ctype
            in
            let operand_irtype = ir_datatype_for_ctype operand_ctype in
            let e1_irtype = ir_datatype_for_ctype e1_ctype in
            let e2_irtype = ir_datatype_for_ctype e2_ctype in
            let e1_ir = Ir.convert operand_irtype e1_irtype e1_ir in
            let e2_ir = Ir.convert operand_irtype e2_irtype e2_ir in

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
            | LogicalNot -> let ctype, e_ir = emit_expr e in (Signed Int, Ir.logical_not e_ir)
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
                (* Lookup function prototype *)
                let proto = match Hashtbl.find_opt prototype_table func_name with
                    | Some p -> p
                    | None -> raise (Compile_error (sprintf "Can't call undeclared function '%s'" func_name))
                in
                let result_dest, result_ir = match proto.ret_type with
                    (* TODO We pretend void has the value 0 because currently
                    the IR has not way to represent void *)
                    | Void -> (None, Ir.ConstInt(Ir.I32, 0L))
                    | _ -> begin
                        let result_local_id = new_local proto.ret_type in
                        let result_irtype = ir_datatype_for_ctype proto.ret_type in
                        (Some (result_irtype, result_local_id), Ir.Load (result_irtype, Ir.LocalAddr result_local_id))
                    end
                in
                let evaluated_args = eval_arguments proto.is_varargs args proto.arg_types in
                add_inst @@ Ir.Call (result_dest, func_name, evaluated_args);

                (proto.ret_type, result_ir)
            end
    in
    (* Put arguments into var_table *)
    List.iteri (fun i (ctype, arg_name) -> Hashtbl.add (List.hd !scopes) arg_name (-(i + 1), ctype)) func_params;

    emit_stmt func_body;
    { Ir.insts = List.rev !insts; Ir.locals = !locals; }

let to_ir decl_list =
    let func_table = Hashtbl.create 64 in
    let func_prototypes = Hashtbl.create 64 in
    let global_ctypes = Hashtbl.create 64 in

    (* Create function prototype table and global variable table *)
    List.iter (fun d ->
        match d with
        | Function (ret_type, name, named_params, _) -> Hashtbl.add func_prototypes name
            { name; ret_type; arg_types=List.map fst named_params; is_varargs=false; is_extern=false }
        | FunctionDecl (ret_type, name, named_params, is_varargs) -> Hashtbl.add func_prototypes name
            (* TODO: A function declaration is not necessarily extern, it
            might be a forward declaration *)
            { name; ret_type; arg_types=List.map fst named_params; is_varargs; is_extern=true }
        | GlobalVarDecl (ctype, name) -> Hashtbl.add global_ctypes name ctype
    ) decl_list;

    (* Create table of function IR *)
    List.iter (fun d ->
        match d with
        | Function (ret_ctype, name, params, body) -> Hashtbl.add func_table name (func_to_ir func_prototypes global_ctypes ret_ctype name params body)
        | FunctionDecl _ | GlobalVarDecl _ -> ()
    ) decl_list;

    {
        Ir.extern_symbols = Hashtbl.fold (fun name proto acc -> if proto.is_extern then name :: acc else acc) func_prototypes [];
        Ir.global_variables = Hashtbl.fold (fun v ctype acc -> (v, {Ir.size=sizeof ctype; Ir.alignment=alignment ctype}) :: acc) global_ctypes [];
        Ir.func_table;
    }

let test_binop_common_ctype () = begin
    assert ((binop_common_ctype (Signed Char) (Signed Char)) = Signed Int);
    assert ((binop_common_ctype (Signed Int) (Unsigned Int)) = Unsigned Int);
    assert ((binop_common_ctype (Signed Long) (Unsigned Int)) = Signed Long);
    assert ((binop_common_ctype (Signed Long) (Unsigned Long)) = Unsigned Long);
    assert ((binop_common_ctype (Double) (Signed Int)) = Double);
    assert ((binop_common_ctype (Double) (Float)) = Double);
    assert ((binop_common_ctype (Float) (Signed Long)) = Float);
end

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
    test_binop_common_ctype ();
    test_ctype_for_integer_literal ();
end