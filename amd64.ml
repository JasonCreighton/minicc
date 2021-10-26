open Printf

exception Compile_error of string

let call_registers = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"]

let id_of_string_lit lit_table s =
    match Hashtbl.find_opt lit_table s with
    | Some id -> id
    | None ->
        let new_id = Hashtbl.length lit_table in
        Hashtbl.add lit_table s new_id;
        new_id

let layout_stack_frame locals =
    let offset_table = Hashtbl.create 100 in
    let rbp_offset = ref 0 in
    (* Sort by alignment to minimize holes *)
    let sorted_by_desc_alignment = List.sort (fun (_, a) (_, b) -> Int.compare b.Ir.alignment a.Ir.alignment) locals in

    List.iter (fun (local_id, local_def) -> begin        
        (* Consume space *)
        rbp_offset := !rbp_offset - local_def.Ir.size;

        (* Align if necessary *)
        rbp_offset := !rbp_offset land (-local_def.Ir.alignment);

        (* Record in offset table *)
        Hashtbl.add offset_table local_id !rbp_offset
    end) sorted_by_desc_alignment;

    (-(!rbp_offset), offset_table)

let acc_reg_name typ =
    match typ with
    | Ir.U8 | Ir.I8 -> "al"
    | Ir.U16 | Ir.I16 -> "ax"
    | Ir.U32 | Ir.I32 -> "eax"
    | Ir.U64 | Ir.I64 | Ir.Ptr -> "rax"

let emit_func func_table lit_table ir_func =
    (* Variables *)
    let out_buffer = Buffer.create 4096 in
    let stack_bytes_allocated, offset_table = layout_stack_frame ir_func.Ir.locals in

    (* Helper functions *)
    let asmf fmt =
        Buffer.add_char out_buffer '\t';
        kbprintf (fun b -> Buffer.add_char b '\n') out_buffer fmt
    in
    let asm_rawf fmt = bprintf out_buffer fmt in
    let asm str = asmf "%s" str in
    let find_local_offset local_id =
        (* HACK: Negative local IDs refer to function arguments *)
        if local_id < 0    
        then ((-local_id) + 1) * 8
        else
            match Hashtbl.find_opt offset_table local_id with
            | Some loc -> loc
            | None -> raise (Compile_error (sprintf "Undeclared local %d" local_id))
    in

    (* Recursive walk functions *)    
    let rec emit_inst inst =
        match inst with
        | Ir.Store (_, address, value) -> begin
            let address_typ = emit_expr address in
            if address_typ <> Ir.Ptr then raise (Compile_error "Address type is not Ptr");
            asm "push rax";
            let value_typ = emit_expr value in
            asm "pop rbx";
            asmf "mov [rbx], %s" (acc_reg_name value_typ)
        end
        | Ir.Call (dest, func_name, arguments) -> begin
            (* Evaluate arguments onto stack *)
            List.iter (fun a -> emit_expr a |> ignore; asm "push rax") (List.rev arguments);

            if Option.is_some (Hashtbl.find_opt func_table func_name) then (
                (* Call one of our functions *)
                asmf "call %s" func_name
            ) else (
                (* Library call *)
                (* Pop stack values into calling convention registers *)
                List.iteri (fun i reg -> if i < (List.length arguments) then asmf "pop %s" reg else ()) call_registers;

                (* FIXME: Terrible hack to align stack to 16 bytes before calling library function, this should be done statically *)
                asm "mov rbx, rsp"; (* Save old stack pointer in callee-save register *)
                asm "and rsp, -16"; (* Align stack *)
                asmf "call %s WRT ..plt" func_name;
                asm "mov rsp, rbx" (* Restore old stack pointer *)
            );

            (* Optionally save result of call to a local *)
            Option.iter (fun (typ, local_id) -> asmf "mov [rbp + %d], %s" (find_local_offset local_id) (acc_reg_name typ)) dest
        end
        | Ir.Label label_id -> asm_rawf ".L%d:\n" label_id
        | Ir.Jump label_id -> asmf "jmp .L%d" label_id
        | Ir.JumpIf (label_id, cond) -> begin
            emit_expr cond |> ignore;
            asm "cmp rax, 0";
            asmf "jne .L%d" label_id
        end
        | Ir.Return expr_opt ->
            Option.iter (fun e -> emit_expr e |> ignore) expr_opt;
            asm "jmp .epilogue"    
    and emit_expr expr =
        (* FIXME *)
        match expr with
        | Ir.ConstInt (typ, n) -> asmf "mov rax, %Ld" n; typ
        | Ir.ConstStringAddr s -> asmf "mov rax, string_lit_%d" (id_of_string_lit lit_table s); Ir.Ptr
        | Ir.LocalAddr local_id -> asmf "lea rax, [rbp + %d]" (find_local_offset local_id); Ir.Ptr
        | Ir.Load (typ, addr) -> begin
            emit_expr addr |> ignore;
            let ld inst dest_reg width = asmf "%s %s, %s [rax]" inst dest_reg width in
            (
            match typ with
                | Ir.I8 -> ld "movsx" "rax" "byte"
                | Ir.I16 -> ld "movsx" "rax" "word"
                | Ir.I32 -> ld "movsx" "rax" "dword"
                | Ir.U8 -> ld "movzx" "rax" "byte"
                | Ir.U16 -> ld "movzx" "rax" "word"
                | Ir.U32 -> ld "mov" "eax" "dword"
                | Ir.I64 | Ir.U64 | Ir.Ptr -> ld "mov" "rax" "qword"
            );

            typ
        end            
        | Ir.BinOp (op, e1, e2) -> begin
            let materialize_comparison condition_code =
                asm "cmp rax, rcx";
                asm "mov rax, 0";
                asm "mov rcx, 1";
                asmf "cmov%s rax, rcx" condition_code
            in
            (*
            Note: Evaluate RHS first, so we can end up with the LHS in rax.
            This makes things nicer for subtraction and division.
            *)
            let rhs_t = emit_expr e2 in
            asm "push rax";
            let lhs_t = emit_expr e1 in
            assert (lhs_t = rhs_t);
            asm "pop rcx";

            (* FIXME: Handle unsigned cases properly *)
            (
            match op with
            | Ir.Add -> asm "add rax, rcx"
            | Ir.Sub -> asm "sub rax, rcx"
            | Ir.Mul -> asm "imul rax, rcx"
            | Ir.Div -> asm "cqo"; asm "idiv rcx"
            | Ir.Rem -> asm "cqo"; asm "idiv rcx"; asm "mov rax, rdx"
            | Ir.And -> asm "and rax, rcx"
            | Ir.Or -> asm "or rax, rcx"
            | Ir.Xor -> asm "xor rax, rcx"
            | Ir.ShiftLeft -> asm "sal rax, cl"
            | Ir.ShiftRight -> asm "sar rax, cl"
            | Ir.CompEQ -> materialize_comparison "e"
            | Ir.CompNEQ -> materialize_comparison "ne"
            | Ir.CompLT -> materialize_comparison "l"
            | Ir.CompLTE -> materialize_comparison "le"
            | Ir.CompGT -> materialize_comparison "g"
            | Ir.CompGTE -> materialize_comparison "ge"
            );

            (* Sign or zero extend as appropriate *)
            (match lhs_t with
            | Ir.I8 -> asm "movsx rax, al"
            | Ir.I16 -> asm "movsx rax, ax"
            | Ir.I32 -> asm "movsx rax, eax"
            | Ir.U8 -> asm "movzx rax, al"
            | Ir.U16 -> asm "movzx rax, ax"
            | Ir.U32 -> asm "mov eax, eax"
            | Ir.I64 | Ir.U64 | Ir.Ptr -> ()
            );

            lhs_t
        end
        | Ir.UnaryOp (op, e) -> begin
            match op with
            | Ir.Not -> let typ = emit_expr e in asm "not rax"; typ
            | Ir.Neg -> let typ = emit_expr e in asm "neg rax"; typ
            | Ir.LogicalNot -> begin
                let typ = emit_expr e in
                asm "test rax, rax";
                asm "sete al";
                asm "movzx rax, al";
                typ
            end
        end
        | Ir.ConvertTo (typ, e) -> emit_expr e |> ignore; typ
    in
    List.iter emit_inst ir_func.Ir.insts;
    (out_buffer, stack_bytes_allocated)

let emit func_table =
    let lit_table = Hashtbl.create 100 in
    let ob = Buffer.create 4096 in

    Buffer.add_string ob "extern printf\n";
    Buffer.add_string ob "section .text\n";

    Hashtbl.iter (fun func_name func_ir -> begin
        let body_buf, stack_bytes_allocated = emit_func func_table lit_table func_ir in

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
    ) func_table;

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