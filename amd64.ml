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

let emit_func body =
    (* Variables *)
    let out_buffer = Buffer.create 4096 in
    let lit_table = Hashtbl.create 100 in
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
    let rec put_in_rax_rbx e1 e2 =
        begin
            (*
            Note: Evaluate RHS first, so we can end up with the LHS in rax.
            This makes things nicer for subtraction and division.
            *)
            emit_expr e2;
            asm "push rax";
            emit_expr e1;
            asm "pop rbx"
        end
    and assign_var v expr =
        emit_expr expr;
        let loc, ctype = find_var_loc v in
        let reg_name = match sizeof ctype with
            | 1 -> "al"
            | 2 -> "ax"
            | 4 -> "eax"
            | 8 -> "rax"
            | _ -> failwith "Should not happen"
        in
        asmf "mov [rbp - %d], %s" loc reg_name
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
    and emit_expr expr =
        match expr with
        | Lit n -> asmf "mov rax, %d" n;
        | LitString s -> asmf "mov rax, string_lit_%d" (id_of_string_lit lit_table s)
        | Assign (VarRef v, rhs) -> assign_var v rhs
        | Assign (_, _) -> raise (Compile_error "Assignment to non-lvalue")
        | VarRef v -> begin
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
            in
            asmf "%s %s, %s [rbp - %d]" inst dest_reg width loc
        end
        | Add (e1, e2) -> (put_in_rax_rbx e1 e2; asm "add rax, rbx")
        | Sub (e1, e2) -> (put_in_rax_rbx e1 e2; asm "sub rax, rbx")
        | Mul (e1, e2) -> (put_in_rax_rbx e1 e2; asm "imul rax, rbx")
        | Div (e1, e2) ->
            begin
                put_in_rax_rbx e1 e2;
                asm "cqo";
                asm "idiv rbx";
            end
        | Neg e1 -> (emit_expr e1; asm "neg rax")
        | Call(func_name, args) ->
            begin
                (* Evaluate arguments onto stack *)
                List.iter (fun a -> emit_expr a; asm "push rax") (List.rev args);

                (* Pop stack values into calling convention registers *)
                List.iteri (fun i reg -> if i < (List.length args) then asmf "pop %s" reg else ()) call_registers;

                (* FIXME: We will have to distinguish between library calls and user-defined calls because of the PLT *)
                asmf "call %s WRT ..plt" func_name;
            end
    in
    emit_stmt body; 
    (out_buffer, lit_table, !stack_bytes_allocated)

let emit oc decl =
    match decl with
    | Function (func_name, func_body) ->
        begin
            let body_buf, lit_table, stack_bytes_allocated = emit_func func_body in

            output_string oc "section .text\n";

            fprintf oc "global %s\n" func_name;
            output_string oc "extern printf\n";
            
            fprintf oc "%s:\n" func_name;
            output_string oc "\tpush rbp\n";
            output_string oc "\tmov rbp, rsp\n";

            (* Stack needs to be 16 byte aligned *)
            let eff_stack_bytes = (((stack_bytes_allocated - 1) / 16) + 1) * 16 in
            fprintf oc "\tsub rsp, %d\n" eff_stack_bytes;

            output_string oc (Buffer.contents body_buf);

            output_string oc "\tmov rax, 0\n";
            output_string oc "\tmov rsp, rbp\n";
            output_string oc "\tpop rbp\n";
            output_string oc "\tret\n";

            (* Output string literals *)
            output_string oc "section .data\n";
            Hashtbl.iter (fun lit id ->
                fprintf oc "string_lit_%d:\n" id;
                output_string oc "db ";
                String.iter (fun c -> fprintf oc "%d, " (Char.code c)) lit;

                (* NUL terminate *)
                output_string oc "0\n";
            ) lit_table;
        end