open Printf
open Ast

exception Compile_error of string

let call_registers = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"]

let id_of_string_lit lit_table s =
    match Hashtbl.find_opt lit_table s with
    | Some id -> id
    | None ->
        let new_id = Hashtbl.length lit_table in
        Hashtbl.add lit_table s new_id;
        new_id

let sizeof ctype =
    let sizeof_int int_size =
        match int_size with
        | Char -> 1
        | Short -> 2
        | Int -> 4
        | Long -> 8
    in
    match ctype with
    | Void -> raise (Compile_error "Can't use void in sized context")
    | Signed x | Unsigned x -> sizeof_int x
    | Float -> 4
    | Double -> 8

let emit_func func_table lit_table params body =
    (* Variables *)
    let out_buffer = Buffer.create 4096 in
    let var_table = Hashtbl.create 100 in
    let stack_bytes_allocated = ref 0 in
    let next_label_id = ref 0 in

    (* Helper functions *)
    let asmf fmt =
        Buffer.add_char out_buffer '\t';
        kbprintf (fun b -> Buffer.add_char b '\n') out_buffer fmt
    in
    let asm_rawf fmt = bprintf out_buffer fmt in
    let asm str = asmf "%s" str in
    let find_var_loc v =
        match Hashtbl.find_opt var_table v with
        | Some loc -> loc
        | None -> raise (Compile_error (sprintf "Undeclared variable '%s'" v))
    in
    let new_label () =
        let l = !next_label_id in
        next_label_id := !next_label_id + 1;
        l
    in
    let decl_var ctype v =
        (*
        TODO: This just allocates space as we go, it would be more efficient to
        allocate the whole stack frame at once so we can minimize padding holes
        *)
        
        let size = sizeof ctype in

        (* Align to size *)
        stack_bytes_allocated := (!stack_bytes_allocated + (size - 1)) land (lnot (size - 1));

        (* Consume space *)
        stack_bytes_allocated := !stack_bytes_allocated + size;

        let loc = !stack_bytes_allocated in

        (* Record in variable table *)
        Hashtbl.add var_table v (loc, ctype)
    in

    (* Recursive walk functions *)
    let rec put_in_rax_rcx e1 e2 =
        begin
            (*
            Note: Evaluate RHS first, so we can end up with the LHS in rax.
            This makes things nicer for subtraction and division.
            *)
            emit_expr e2;
            asm "push rax";
            emit_expr e1;
            asm "pop rcx"
        end
    and store_accumulator_to_var v =
        let loc, ctype = find_var_loc v in
        let reg_name = match sizeof ctype with
            | 1 -> "al"
            | 2 -> "ax"
            | 4 -> "eax"
            | 8 -> "rax"
            | _ -> failwith "Should not happen"
        in
        asmf "mov [rbp - %d], %s" loc reg_name
    and load_var_to_accumulator v =
        let loc, ctype = find_var_loc v in
        let inst, dest_reg, width = match ctype with
            | Signed Char -> ("movsx", "rax", "byte")
            | Signed Short -> ("movsx", "rax", "word")
            | Signed Int -> ("movsx", "rax", "dword")
            | Signed Long -> ("mov", "rax", "qword")
            | Unsigned Char -> ("movzx", "rax", "byte")
            | Unsigned Short -> ("movzx", "rax", "word")
            | Unsigned Int -> ("mov", "eax", "dword")
            | Unsigned Long -> ("mov", "rax", "qword")
            | Void | Float | Double -> failwith "TODO: Implement more types"
        in
        asmf "%s %s, %s [rbp - %d]" inst dest_reg width loc
    and assign_var v expr =
        emit_expr expr;
        store_accumulator_to_var v
    and inc_or_dec yield_old_value inst_name expr =
        match expr with
        | VarRef v -> begin
            load_var_to_accumulator v;

            if yield_old_value then asm "mov rcx, rax";

            asmf "%s rax" inst_name; (* Increment or decrement *)
            store_accumulator_to_var v;

            if yield_old_value then asm "mov rax, rcx";
        end
        (* TODO: This is a ugly way to clear the fragile pattern match warning *)
        |Lit _|LitString _|Assign (_, _)|BinOp (_, _, _)|UnaryOp (_, _)|Conditional (_, _, _)|Sequence (_, _)|LogicalAnd (_, _)|LogicalOr (_, _)|Call (_, _) -> raise (Compile_error "Pre-Increment/Decrement of non-lvalue")
    and emit_stmt stmt =
        match stmt with
        | DeclVar (ctype, v) -> decl_var ctype v
        | DeclAssign (ctype, v, expr) -> decl_var ctype v; assign_var v expr
        | ExprStmt expr -> emit_expr expr
        | CompoundStmt stmts -> List.iter emit_stmt stmts
        | IfElseStmt (cond, then_stmt, else_stmt) -> begin
            let else_label_id = new_label () in
            let done_label_id = new_label () in
            emit_expr cond;
            asm "cmp rax, 0";
            asmf "je .label_%d" else_label_id;
            emit_stmt then_stmt;
            asmf "jmp .label_%d" done_label_id;
            asm_rawf ".label_%d: ; else\n" else_label_id;
            emit_stmt else_stmt;
            asm_rawf ".label_%d: ; end if\n" done_label_id
        end
        | WhileStmt (cond, body) -> begin           
            let test_label_id = new_label () in
            let end_label_id = new_label () in

            (* Loop test *)
            asm_rawf ".label_%d: ; while test\n" test_label_id;
            emit_expr cond;
            asm "cmp rax, 0";
            asmf "je .label_%d" end_label_id;

            (* Loop body *)
            emit_stmt body;

            asmf "jmp .label_%d" test_label_id;
            asm_rawf ".label_%d: ; end while\n" end_label_id;
        end
        | ReturnStmt expr_opt ->
            Option.iter emit_expr expr_opt;
            asm "jmp .epilogue"
    and emit_expr expr =
        match expr with
        | Lit n -> asmf "mov rax, %d" n;
        | LitString s -> asmf "mov rax, string_lit_%d" (id_of_string_lit lit_table s)
        | Assign (VarRef v, rhs) -> assign_var v rhs
        | Assign (_, _) -> raise (Compile_error "Assignment to non-lvalue")
        | VarRef v -> load_var_to_accumulator v
        | BinOp (op, e1, e2) -> begin
            let materialize_comparison condition_code =
                asm "cmp rax, rcx";
                asm "mov rax, 0";
                asm "mov rcx, 1";
                asmf "cmov%s rax, rcx" condition_code
            in
            put_in_rax_rcx e1 e2;

            (* FIXME: Handle unsigned cases properly *)
            match op with
            | Add -> asm "add rax, rcx"
            | Sub -> asm "sub rax, rcx"
            | Mul -> asm "imul rax, rcx"
            | Div -> asm "cqo"; asm "idiv rcx"
            | Rem -> asm "cqo"; asm "idiv rcx"; asm "mov rax, rdx"
            | BitAnd -> asm "and rax, rcx"
            | BitOr -> asm "or rax, rcx"
            | BitXor -> asm "xor rax, rcx"
            | BitShiftLeft -> asm "sal rax, cl"
            | BitShiftRight -> asm "sar rax, cl"
            | CompEQ -> materialize_comparison "e"
            | CompNEQ -> materialize_comparison "ne"
            | CompLT -> materialize_comparison "l"
            | CompGT -> materialize_comparison "g"
            | CompLTE -> materialize_comparison "le"
            | CompGTE -> materialize_comparison "ge"

        end
        | UnaryOp (op, e) -> begin
            match op with
            | PreInc -> inc_or_dec false "inc" e
            | PreDec -> inc_or_dec false "dec" e
            | PostInc -> inc_or_dec true "inc" e
            | PostDec -> inc_or_dec true "dec" e
            | BitNot | LogicalNot -> emit_expr e; asm "not rax"
            | Neg -> emit_expr e; asm "neg rax"
        end
        | Conditional (cond, true_expr, false_expr) -> begin
            (* FIXME: Basically identical to IfElseStmt, except with expr instead of stmt *)
            let else_label_id = new_label () in
            let done_label_id = new_label () in
            emit_expr cond;
            asm "cmp rax, 0";
            asmf "je .label_%d" else_label_id;
            emit_expr true_expr;
            asmf "jmp .label_%d" done_label_id;
            asm_rawf ".label_%d: ; ternary false branch\n" else_label_id;
            emit_expr false_expr;
            asm_rawf ".label_%d: ; end ternary\n" done_label_id
        end
        | Sequence (e1, e2) -> emit_expr e1; emit_expr e2
        | LogicalAnd (e1, e2) -> begin
            (* FIXME: Very similar to LogicalOr *)
            let short_circuit_label_id = new_label () in
            emit_expr e1;
            asm "cmp rax, 0";
            asmf "je .label_%d" short_circuit_label_id;
            emit_expr e2;
            asm_rawf ".label_%d: ; short circuit &&\n" short_circuit_label_id
        end
        | LogicalOr (e1, e2) -> begin
            (* FIXME: Very similar to LogicalAnd *)
            let short_circuit_label_id = new_label () in
            emit_expr e1;
            asm "cmp rax, 0";
            asmf "jne .label_%d" short_circuit_label_id;
            emit_expr e2;
            asm_rawf ".label_%d: ; short circuit ||\n" short_circuit_label_id
        end
        | Call(func_name, args) ->
            begin
                (* Evaluate arguments onto stack *)
                List.iter (fun a -> emit_expr a; asm "push rax") (List.rev args);

                if Option.is_some (Hashtbl.find_opt func_table func_name) then (
                    (* Call one of our functions *)
                    asmf "call %s" func_name
                ) else (
                    (* Library call *)
                    (* Pop stack values into calling convention registers *)
                    List.iteri (fun i reg -> if i < (List.length args) then asmf "pop %s" reg else ()) call_registers;
                    asmf "call %s WRT ..plt" func_name;
                )
            end
    in
    (* Put arguments into var_table *)
    List.iteri (fun i (ctype, arg_name) -> Hashtbl.add var_table arg_name (-(i+2)*8, ctype)) params;
    emit_stmt body; 
    (out_buffer, !stack_bytes_allocated)

let emit oc decl_list =
    let func_table = Hashtbl.create 64 in
    let lit_table = Hashtbl.create 100 in
    List.iter (fun d ->
        match d with
        | Function (_, name, _, _) as func -> Hashtbl.add func_table name func
        | FunctionDecl _ -> ()
    ) decl_list;

    output_string oc "extern printf\n";
    output_string oc "section .text\n";

    List.iter (fun decl ->
        match decl with
        | Function (_, func_name, func_params, func_body) ->
            begin
                let body_buf, stack_bytes_allocated = emit_func func_table lit_table func_params func_body in

                fprintf oc "global %s\n" func_name;
                fprintf oc "%s:\n" func_name;
                output_string oc "\tpush rbp\n";
                output_string oc "\tmov rbp, rsp\n";

                (* Stack needs to be 16 byte aligned *)
                let eff_stack_bytes = (((stack_bytes_allocated - 1) / 16) + 1) * 16 in
                fprintf oc "\tsub rsp, %d\n" eff_stack_bytes;

                output_string oc (Buffer.contents body_buf);

                output_string oc ".epilogue:\n";
                output_string oc "\tmov rsp, rbp\n";
                output_string oc "\tpop rbp\n";
                output_string oc "\tret\n";

            end
        | FunctionDecl _ -> ()
    ) decl_list;

    (* Output string literals *)
    output_string oc "section .data\n";
    Hashtbl.iter (fun lit id ->
        fprintf oc "string_lit_%d:\n" id;
        output_string oc "db ";
        String.iter (fun c -> fprintf oc "%d, " (Char.code c)) lit;

        (* NUL terminate *)
        output_string oc "0\n";
    ) lit_table;