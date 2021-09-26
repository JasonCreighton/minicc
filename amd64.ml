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

let rec sizeof ctype =
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
    | PointerTo _ -> 8
    | ArrayOf (ctype, size) -> (sizeof ctype) * size

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
    let acc_reg_name ctype =
        let eff_size =
            match ctype with
            (* In this context, it is really a pointer to array contents that we are loading or storing *)
            | ArrayOf (_, _) -> 8
            | _ -> sizeof ctype
        in
        match eff_size with
        | 1 -> "al"
        | 2 -> "ax"
        | 4 -> "eax"
        | 8 -> "rax"
        | _ -> failwith "Should not happen"
    in

    (* Recursive walk functions *)
    let rec put_in_rax_rcx e1 e2 =
        begin
            (*
            Note: Evaluate RHS first, so we can end up with the LHS in rax.
            This makes things nicer for subtraction and division.
            *)
            emit_expr e2 |> ignore;
            asm "push rax";
            emit_expr e1 |> ignore;
            asm "pop rcx"
        end
    and store_accumulator_to_var v =
        let loc, ctype = find_var_loc v in
        asmf "mov [rbp - %d], %s" loc (acc_reg_name ctype)
    and load_var_to_accumulator v =
        let loc, ctype = find_var_loc v in
        let ld inst dest_reg width = asmf "%s %s, %s [rbp - %d]" inst dest_reg width loc in
        (
        match ctype with
            | Signed Char -> ld "movsx" "rax" "byte"
            | Signed Short -> ld "movsx" "rax" "word"
            | Signed Int -> ld "movsx" "rax" "dword"
            | Signed Long -> ld "mov" "rax" "qword"
            | Unsigned Char -> ld "movzx" "rax" "byte"
            | Unsigned Short -> ld "movzx" "rax" "word"
            | Unsigned Int -> ld "mov" "eax" "dword"
            | Unsigned Long -> ld "mov" "rax" "qword"
            | PointerTo _ -> ld "mov" "rax" "qword"
            | ArrayOf (_, _) -> asmf "lea rax, [rbp - %d]" loc
            | Void | Float | Double -> failwith "TODO: Implement more types"
        );
        ctype
    and load_address_of_lvalue expr =
        match expr with
        | VarRef v -> let loc, ctype = find_var_loc v in asmf "lea rax, [rbp - %d]" loc; ctype
        | Subscript (ary, idx) -> begin
            let ArrayOf (elem_ctype, ary_len) = load_address_of_lvalue ary in
            asm "push rax";
            emit_expr idx |> ignore;
            asmf "imul rax, %d" (sizeof elem_ctype);
            asm "pop rbx";
            asm "lea rax, [rbx + rax]";
            elem_ctype
        end
        | Deref addr_expr ->
            let PointerTo ctype = load_address_of_lvalue addr_expr in
            asm "mov rax, [rax]";
            ctype
        | _ -> raise (Compile_error "expr is not an lvalue")
    and assign_var v expr =
        let _, ctype = find_var_loc v in
        emit_expr expr |> ignore;
        store_accumulator_to_var v;
        ctype
    and inc_or_dec yield_old_value inst_name expr =
        match expr with
        | VarRef v -> begin
            let ctype = load_var_to_accumulator v in

            if yield_old_value then asm "mov rcx, rax";

            asmf "%s rax" inst_name; (* Increment or decrement *)
            store_accumulator_to_var v;

            if yield_old_value then asm "mov rax, rcx";

            ctype
        end
        | Subscript (_, _) -> failwith "TODO: Implement array inc/dec"
        (* TODO: This is a ugly way to clear the fragile pattern match warning *)
        |Lit _|LitString _|Assign (_, _)|BinOp (_, _, _)|UnaryOp (_, _)|Conditional (_, _, _)|Sequence (_, _)|LogicalAnd (_, _)|LogicalOr (_, _)|Call (_, _) -> raise (Compile_error "Pre-Increment/Decrement of non-lvalue")
    and emit_loop init_opt cond incr_opt body = begin
        let test_label_id = new_label () in
        let end_label_id = new_label () in

        Option.iter (fun e -> emit_expr e |> ignore) init_opt;

        (* Loop test *)
        asm_rawf ".label_%d: ; loop test\n" test_label_id;
        emit_expr cond |> ignore;
        asm "cmp rax, 0";
        asmf "je .label_%d" end_label_id;

        (* Loop body *)
        emit_stmt body;

        (* Loop increment *)
        Option.iter (fun e -> emit_expr e |> ignore) incr_opt;

        asmf "jmp .label_%d" test_label_id;
        asm_rawf ".label_%d: ; end while\n" end_label_id;
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
            emit_expr cond |> ignore;
            asm "cmp rax, 0";
            asmf "je .label_%d" else_label_id;
            emit_stmt then_stmt;
            asmf "jmp .label_%d" done_label_id;
            asm_rawf ".label_%d: ; else\n" else_label_id;
            emit_stmt else_stmt;
            asm_rawf ".label_%d: ; end if\n" done_label_id
        end
        | WhileStmt (cond, body) -> emit_loop Option.none cond Option.none body
        | ForStmt (init, cond, incr, body) -> emit_loop (Option.some init) cond (Option.some incr) body
        | ReturnStmt expr_opt ->
            Option.iter (fun e -> emit_expr e |> ignore) expr_opt;
            asm "jmp .epilogue"    
    and emit_expr expr =
        match expr with
        | Lit n -> asmf "mov rax, %d" n; Signed Long
        | LitString s -> asmf "mov rax, string_lit_%d" (id_of_string_lit lit_table s); PointerTo (Unsigned Char)
        | Assign (VarRef v, rhs) -> assign_var v rhs
        | Assign (lvalue, rhs) -> begin
            let lhs_ctype = load_address_of_lvalue lvalue in
            asm "push rax";
            let rhs_ctype = emit_expr rhs in
            asm "pop rbx";
            asmf "mov [rbx], %s" (acc_reg_name lhs_ctype);
            rhs_ctype
        end
        | VarRef v -> load_var_to_accumulator v
        | Subscript (_, _) -> begin
            let elem_ctype = load_address_of_lvalue expr in
            asmf "mov %s, [rax]" (acc_reg_name elem_ctype); (* FIXME: Seems like I should have to deal with sign extension here? *)
            elem_ctype
        end
        | Deref e -> let PointerTo ctype = emit_expr e in asmf "mov %s, [rax]" (acc_reg_name ctype); ctype
        | BinOp (op, e1, e2) -> begin
            let materialize_comparison condition_code =
                asm "cmp rax, rcx";
                asm "mov rax, 0";
                asm "mov rcx, 1";
                asmf "cmov%s rax, rcx" condition_code
            in
            put_in_rax_rcx e1 e2;

            (* FIXME: Handle unsigned cases properly *)
            (
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
            );

            Signed Long (* FIXME: Should not hardcode *)

        end
        | UnaryOp (op, e) -> begin
            match op with
            | PreInc -> inc_or_dec false "inc" e
            | PreDec -> inc_or_dec false "dec" e
            | PostInc -> inc_or_dec true "inc" e
            | PostDec -> inc_or_dec true "dec" e
            | BitNot | LogicalNot -> let ctype = emit_expr e in asm "not rax"; ctype
            | Neg -> let ctype = emit_expr e in asm "neg rax"; ctype
            | AddressOf -> PointerTo (load_address_of_lvalue e)
        end
        | Conditional (cond, true_expr, false_expr) -> begin
            (* FIXME: Basically identical to IfElseStmt, except with expr instead of stmt *)
            let else_label_id = new_label () in
            let done_label_id = new_label () in
            emit_expr cond |> ignore;
            asm "cmp rax, 0";
            asmf "je .label_%d" else_label_id;
            emit_expr true_expr |> ignore;
            asmf "jmp .label_%d" done_label_id;
            asm_rawf ".label_%d: ; ternary false branch\n" else_label_id;
            emit_expr false_expr |> ignore;
            asm_rawf ".label_%d: ; end ternary\n" done_label_id;

            Signed Long (* FIXME: Should not hardcode *)
        end
        | Sequence (e1, e2) -> emit_expr e1 |> ignore; emit_expr e2
        | LogicalAnd (e1, e2) -> begin
            (* FIXME: Very similar to LogicalOr *)
            let short_circuit_label_id = new_label () in
            emit_expr e1 |> ignore;
            asm "cmp rax, 0";
            asmf "je .label_%d" short_circuit_label_id;
            emit_expr e2 |> ignore;
            asm_rawf ".label_%d: ; short circuit &&\n" short_circuit_label_id;

            Signed Long (* FIXME: Is this right? *)
        end
        | LogicalOr (e1, e2) -> begin
            (* FIXME: Very similar to LogicalAnd *)
            let short_circuit_label_id = new_label () in
            emit_expr e1 |> ignore;
            asm "cmp rax, 0";
            asmf "jne .label_%d" short_circuit_label_id;
            emit_expr e2 |> ignore;
            asm_rawf ".label_%d: ; short circuit ||\n" short_circuit_label_id;

            Signed Long (* FIXME: Is this right? *)
        end
        | Call(func_name, args) ->
            begin
                (* Evaluate arguments onto stack *)
                List.iter (fun a -> emit_expr a |> ignore; asm "push rax") (List.rev args);

                if Option.is_some (Hashtbl.find_opt func_table func_name) then (
                    (* Call one of our functions *)
                    asmf "call %s" func_name
                ) else (
                    (* Library call *)
                    (* Pop stack values into calling convention registers *)
                    List.iteri (fun i reg -> if i < (List.length args) then asmf "pop %s" reg else ()) call_registers;

                    (* FIXME: Terrible hack to align stack to 16 bytes before calling library function, this should be done statically *)
                    asm "mov rbx, rsp"; (* Save old stack pointer in callee-save register *)
                    asm "and rsp, -16"; (* Align stack *)
                    asmf "call %s WRT ..plt" func_name;
                    asm "mov rsp, rbx" (* Restore old stack pointer *)
                );

                Signed Long (* FIXME: Should lookup from function declaration or definition *)
            end
    in
    (* Put arguments into var_table *)
    List.iteri (fun i (ctype, arg_name) -> Hashtbl.add var_table arg_name (-(i+2)*8, ctype)) params;
    emit_stmt body;
    (out_buffer, !stack_bytes_allocated)

let emit decl_list =
    let func_table = Hashtbl.create 64 in
    let lit_table = Hashtbl.create 100 in
    let ob = Buffer.create 4096 in
    List.iter (fun d ->
        match d with
        | Function (_, name, _, _) as func -> Hashtbl.add func_table name func
        | FunctionDecl _ -> ()
    ) decl_list;

    Buffer.add_string ob "extern printf\n";
    Buffer.add_string ob "section .text\n";

    List.iter (fun decl ->
        match decl with
        | Function (_, func_name, func_params, func_body) ->
            begin
                let body_buf, stack_bytes_allocated = emit_func func_table lit_table func_params func_body in

                bprintf ob "global %s\n" func_name;
                bprintf ob "%s:\n" func_name;
                Buffer.add_string ob "\tpush rbp\n";
                Buffer.add_string ob "\tmov rbp, rsp\n";

                (* Stack needs to be 8 byte aligned *)
                let eff_stack_bytes = ((stack_bytes_allocated + 7) / 8)  * 8 in
                if eff_stack_bytes > 0 then bprintf ob "\tsub rsp, %d\n" eff_stack_bytes;

                Buffer.add_buffer ob body_buf;

                Buffer.add_string ob ".epilogue:\n";
                Buffer.add_string ob "\tmov rsp, rbp\n";
                Buffer.add_string ob "\tpop rbp\n";
                Buffer.add_string ob "\tret\n";

            end
        | FunctionDecl _ -> ()
    ) decl_list;

    (* Output string literals *)
    Buffer.add_string ob "section .data\n";
    Hashtbl.iter (fun lit id ->
        bprintf ob "string_lit_%d:\n" id;
        Buffer.add_string ob "db ";
        String.iter (fun c -> bprintf ob "%d, " (Char.code c)) lit;

        (* NUL terminate *)
        Buffer.add_string ob "0\n";
    ) lit_table;

    (* Return buffer *)
    ob