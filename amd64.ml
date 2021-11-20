open Printf

exception Compile_error of string

type int_width =
    | W8
    | W16
    | W32
    | W64

type float_width =
    | F32
    | F64

module IntReg = struct
    type t = Virt of int [@@unboxed]
    let qword_names = [|"rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15";|]
    let dword_names = [|"eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi"; "r8d"; "r9d"; "r10d"; "r11d"; "r12d"; "r13d"; "r14d"; "r15d";|]
    let word_names = [|"ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"; "r8w"; "r9w"; "r10w"; "r11w"; "r12w"; "r13w"; "r14w"; "r15w";|]
    let byte_names = [|"al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil"; "r8b"; "r9b"; "r10b"; "r11b"; "r12b"; "r13b"; "r14b"; "r15b";|]

    let to_string width (Virt n) = begin
        assert (n >= 0);
        if n < 16 then
            match width with
            | W64 -> qword_names.(n)
            | W32 -> dword_names.(n)
            | W16 -> word_names.(n)
            | W8 -> byte_names.(n)
        else
            "v" ^ Int.to_string n
    end

    let rax = Virt 0
    let rcx = Virt 1
    let rdx = Virt 2
    let rbx = Virt 3
    let rsp = Virt 4
    let rbp = Virt 5
    let rsi = Virt 6
    let rdi = Virt 7
    let r8 = Virt 8
    let r9 = Virt 9
    let r10 = Virt 10
    let r11 = Virt 11
    let r12 = Virt 12
    let r13 = Virt 13
    let r14 = Virt 14
    let r15 = Virt 15
end

module XmmReg = struct
    type t = Virt of int [@@unboxed]

    let to_string (Virt n) = begin
        assert (n >= 16);
        if n < 32 then
            "xmm" ^ (Int.to_string (n - 16))
        else
            "v" ^ Int.to_string n
    end

    let xmm0 = Virt 16
    let xmm1 = Virt 17
    let xmm2 = Virt 18
    let xmm3 = Virt 19
    let xmm4 = Virt 20
    let xmm5 = Virt 21
    let xmm6 = Virt 22
    let xmm7 = Virt 23
    let xmm8 = Virt 24
    let xmm9 = Virt 25
    let xmm10 = Virt 26
    let xmm11 = Virt 27
    let xmm12 = Virt 28
    let xmm13 = Virt 29
    let xmm14 = Virt 30
    let xmm15 = Virt 31
end

type mem_address =
    | AddrB of {base: IntReg.t;}
    | AddrBD of {base: IntReg.t; disp: int32;}
    | AddrSIBD of {scale: int; index: IntReg.t; base: IntReg.t; disp: int32;}
    | AddrSymbol of string

type 'a reg_mem =
    | Reg of 'a
    | Memory of mem_address

type reg_mem_imm =
    | Imm32 of int32
    | RegMem of IntReg.t reg_mem

type ibinop =
    | Add
    | Sub
    | Mul
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Sar

type iunaryop =
    | Neg
    | Not

type fbinop =
    | FAdd
    | FSub
    | FMul
    | FDiv
    | FXor

type label_id = int
type condition_code = string

type inst =
    | UnaryOp of iunaryop * int_width * IntReg.t reg_mem
    | BinOp of ibinop * int_width * IntReg.t reg_mem * reg_mem_imm
    | FBinOp of fbinop * float_width * XmmReg.t * XmmReg.t reg_mem
    | CvtFloatToFloat of float_width * XmmReg.t * float_width * XmmReg.t reg_mem
    | CvtFloatToInt of int_width * IntReg.t * float_width * XmmReg.t reg_mem
    | CvtIntToFloat of float_width * XmmReg.t * int_width * IntReg.t reg_mem
    | Mov of int_width * IntReg.t reg_mem * reg_mem_imm
    | MovImm64 of IntReg.t * int64
    | Lea of IntReg.t * mem_address
    | SignExtendRaxToRdx of int_width (* cqo/cdq *)
    | Div of int_width * IntReg.t reg_mem
    | UnsignedDiv of int_width * IntReg.t reg_mem
    | Cmp of int_width * IntReg.t reg_mem * reg_mem_imm
    | Comi of float_width * XmmReg.t * XmmReg.t reg_mem
    | Label of label_id
    | Jmp of label_id
    | Jcc of condition_code * label_id
    | Setcc of condition_code * IntReg.t

let ibinop_to_string op =
    match op with
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Shl -> "shl"
    | Shr -> "shr"
    | Sar -> "sar"

let iunaryop_to_string op =
    match op with
    | Not -> "not"
    | Neg -> "neg"

let float_width_to_string width =
    match width with
    | F32 -> "ss"
    | F64 -> "sd"

let fbinop_to_string op width =
    let suffix = float_width_to_string width in
    match op with
    | FAdd -> "add" ^ suffix
    | FSub -> "sub" ^ suffix
    | FMul -> "mul" ^ suffix
    | FDiv -> "div" ^ suffix
    | FXor -> "xor" ^ suffix

let mem_address_to_string m =
    match m with
    | AddrB {base} -> sprintf "[%s]" (IntReg.to_string W64 base)
    | AddrBD {base; disp} -> sprintf "[%s + %ld]" (IntReg.to_string W64 base) disp
    | AddrSIBD {scale; index; base; disp} -> sprintf "[%s + %d*%s + %ld]" (IntReg.to_string W64 base) scale (IntReg.to_string W64 index) disp
    | AddrSymbol sym -> sprintf "[%s]" sym

let int_reg_mem_to_string width operand =
    match operand with
    | Reg r -> IntReg.to_string width r
    | Memory m -> mem_address_to_string m

let int_reg_mem_imm_to_string width operand =
    match operand with
    | Imm32 imm -> Int32.to_string imm
    | RegMem regmem -> int_reg_mem_to_string width regmem

let xmm_reg_mem_to_string operand =
    match operand with
    | Reg r -> XmmReg.to_string r
    | Memory m -> mem_address_to_string m

let cqo_or_cdq width =
    match width with
    | W64 -> "cqo"
    | W32 -> "cdq"
    | W16 | W8 -> failwith "Can only do 64-bit or 32-bit divides right now"

let inst_to_buffer buf inst = begin
    match inst with
    | UnaryOp (op, width, operand) -> bprintf buf "\t%s %s\n" (iunaryop_to_string op) (int_reg_mem_to_string width operand)
    | BinOp (op, width, lhs, rhs) -> bprintf buf "\t%s %s, %s\n" (ibinop_to_string op) (int_reg_mem_to_string width lhs) (int_reg_mem_imm_to_string width rhs)
    | FBinOp (op, width, lhs, rhs) -> bprintf buf "\t%s %s, %s\n" (fbinop_to_string op width) (XmmReg.to_string lhs) (xmm_reg_mem_to_string rhs)
    | CvtFloatToFloat (dest_width, dest, src_width, src) -> bprintf buf "\tcvt%s%,2%s %s, %s\n" (float_width_to_string dest_width) (float_width_to_string dest_width) (XmmReg.to_string dest) (xmm_reg_mem_to_string src)
    | CvtFloatToInt (dest_width, dest, src_width, src) -> bprintf buf "\tcvt%s%,2si %s, %s\n" (float_width_to_string src_width) (IntReg.to_string dest_width dest) (xmm_reg_mem_to_string src)
    | CvtIntToFloat (dest_width, dest, src_width, src) -> bprintf buf "\tcvtsi2%s %s, %s\n" (float_width_to_string dest_width) (XmmReg.to_string dest) (int_reg_mem_to_string src_width src)
    | Mov (width, dest, src) -> bprintf buf "\tmov %s, %s" (int_reg_mem_to_string width dest) (int_reg_mem_imm_to_string width src)
    | MovImm64 (dest, imm) -> bprintf buf "\tmov %s, %Ld" (IntReg.to_string W64 dest) imm
    | Lea (dest, addr) -> bprintf buf "\tlea %s, %s" (IntReg.to_string W64 dest) (mem_address_to_string addr)
    | SignExtendRaxToRdx int_width -> bprintf buf "\t%s\n" (cqo_or_cdq int_width)
    | Div (width, divisor) -> bprintf buf "\tidiv %s\n" (int_reg_mem_to_string width divisor)
    | UnsignedDiv (width, divisor) -> bprintf buf "\tdiv %s\n" (int_reg_mem_to_string width divisor)
    | Cmp (width, lhs, rhs) -> bprintf buf "\tcmp %s, %s\n" (int_reg_mem_to_string width lhs) (int_reg_mem_imm_to_string width rhs)
    | Comi (width, lhs, rhs) -> bprintf buf "\tcomi%s %s, %s\n" (float_width_to_string width) (XmmReg.to_string lhs) (xmm_reg_mem_to_string rhs)
    | Label label_id -> bprintf buf ".L%d:\n" label_id
    | Jmp label_id -> bprintf buf "\tjmp .L%d:\n" label_id
    | Jcc (condition_code, label_id) -> bprintf buf "\tj%s .L%d:\n" condition_code label_id
    | Setcc (condition_code, dest) -> bprintf buf "\tset%s %s\n" condition_code (IntReg.to_string W8 dest)
end

let integer_call_registers = [|"rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"|]
let float_call_registers = [|"xmm0"; "xmm1"; "xmm2"; "xmm3"; "xmm4"; "xmm5"; "xmm6"; "xmm7"|]

type constant_tables = {
    strings: (string, int) Hashtbl.t;
    dwords: (int32, unit) Hashtbl.t;
    qwords: (int64, unit) Hashtbl.t;
}

let id_of_string_lit lit_table s =
    match Hashtbl.find_opt lit_table s with
    | Some id -> id
    | None ->
        let new_id = Hashtbl.length lit_table in
        Hashtbl.add lit_table s new_id;
        new_id

let address_of_constant_qword const_table n =
    Hashtbl.replace const_table n ();
    sprintf "__minicc_constant_qword_%Lu" n

let address_of_constant_dword const_table n =
    Hashtbl.replace const_table n ();
    sprintf "__minicc_constant_dword_%lu" n

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
    | Ir.F32 | Ir.F64 -> "xmm0"

let scratch_reg_name typ =
    match typ with
    | Ir.U8 | Ir.I8 | Ir.U16 | Ir.I16 | Ir.U32 | Ir.I32 | Ir.U64 | Ir.I64 | Ir.Ptr -> "rcx"
    | Ir.F32 | Ir.F64 -> "xmm1"

let emit_func func_table constants ir_func =
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
    let materialize_comparison typ condition_code = begin
        (match typ with
        | Ir.F32 -> asm "comiss xmm0, xmm1"
        | Ir.F64 -> asm "comisd xmm0, xmm1"
        | _ -> asm "cmp rax, rcx"
        );

        asmf "set%s al" condition_code;
        asm "movzx rax, al"
    end in
    let push_acc typ = begin
        if Ir.is_integer typ
        then asm "push rax"
        else (asm "sub rsp, 8"; asm "movsd [rsp], xmm0")
    end in
    let pop typ dest = begin
        if Ir.is_integer typ
        then asmf "pop %s" dest
        else (asmf "movsd %s, [rsp]" dest; asm "add rsp, 8")
    end in
    let store_acc typ dest =
        match typ with
        | Ir.F64 -> asmf "movsd %s, xmm0" dest
        | Ir.F32 -> asmf "movss %s, xmm0" dest
        | _ -> asmf "mov %s, %s" dest (acc_reg_name typ)
    in
    (* Recursive walk functions *)    
    let rec emit_inst inst =
        match inst with
        | Ir.Store (store_typ, address, value) -> begin
            let address_typ = emit_expr address in
            if address_typ <> Ir.Ptr then raise (Compile_error "Address type is not Ptr");
            asm "push rax";
            let value_typ = emit_expr value in
            assert (store_typ = value_typ);
            asm "pop rbx";
            store_acc value_typ "[rbx]";
        end
        | Ir.Call (dest, func_name, arguments) -> begin
            (* Evaluate arguments onto stack *)
            List.iter (fun a -> push_acc (emit_expr a)) (List.rev arguments);

            if Option.is_some (Hashtbl.find_opt func_table func_name) then (
                (* Call one of our functions *)
                asmf "call %s" func_name
            ) else (
                (* Library call *)
                (* Pop stack values into calling convention registers *)
                let int_idx = ref 0 in
                let float_idx = ref 0 in
                List.iteri (fun i arg ->
                    let arg_t = Ir.typecheck_expr arg in
                    let dest_reg = match arg_t with
                    | Ir.F64 | Ir.F32 -> let r = Array.get float_call_registers !float_idx in float_idx := !float_idx + 1; r
                    | _ -> let r = Array.get integer_call_registers !int_idx in int_idx := !int_idx + 1; r
                    in
                    pop arg_t dest_reg
                ) arguments;

                (* FIXME: Terrible hack to align stack to 16 bytes before calling library function, this should be done statically *)
                asm "mov rbx, rsp"; (* Save old stack pointer in callee-save register *)
                asm "and rsp, -16"; (* Align stack *)

                 (* Varargs functions need the number of vector registers used in "al". TODO: Only do this when calling varargs functions. *)
                asmf "mov eax, %d" !float_idx;
                asmf "call %s WRT ..plt" func_name;
                asm "mov rsp, rbx" (* Restore old stack pointer *)
            );

            (* Optionally save result of call to a local *)
            Option.iter (fun (typ, local_id) -> store_acc typ (sprintf "[rbp + %d]" (find_local_offset local_id))) dest
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
    and emit_int_binop op e1 e2 =
        (*
        Note: Evaluate RHS first, so we can end up with the LHS in rax.
        This makes things nicer for subtraction and division.
        *)
        let rhs_t = emit_expr e2 in
        asm "push rax";
        let lhs_t = emit_expr e1 in
        assert (lhs_t = rhs_t);
        asm "pop rcx";

        let signed = Ir.is_signed lhs_t in
        (match op with
        | Ir.Add -> asm "add rax, rcx"
        | Ir.Sub -> asm "sub rax, rcx"
        | Ir.Mul -> asm "imul rax, rcx"
        | Ir.Div ->
            if signed
            then (asm "cqo"; asm "idiv rcx")
            else (asm "xor edx, edx"; asm "div rcx")
        | Ir.Rem ->
            if signed
            then (asm "cqo"; asm "idiv rcx"; asm "mov rax, rdx")
            else (asm "xor edx, edx"; asm "div rcx"; asm "mov rax, rdx")
        | Ir.And -> asm "and rax, rcx"
        | Ir.Or -> asm "or rax, rcx"
        | Ir.Xor -> asm "xor rax, rcx"
        | Ir.ShiftLeft -> asm "sal rax, cl"
        | Ir.ShiftRight -> asmf "%s rax, cl" (if signed then "sar" else "shr")
        | Ir.CompEQ -> materialize_comparison lhs_t "e"
        | Ir.CompNEQ -> materialize_comparison lhs_t "ne"
        | Ir.CompLT -> materialize_comparison lhs_t (if signed then "l" else "b")
        | Ir.CompLTE -> materialize_comparison lhs_t (if signed then "le" else "be")
        | Ir.CompGT -> materialize_comparison lhs_t (if signed then "g" else "a")
        | Ir.CompGTE -> materialize_comparison lhs_t (if signed then "ge" else "ae")
        );

        let result_t = match op with
            | Ir.CompEQ | Ir.CompNEQ | Ir.CompLT | Ir.CompLTE | Ir.CompGT | Ir.CompGTE -> Ir.I32
            | _ -> lhs_t
        in

        (* Sign or zero extend as appropriate *)
        (match result_t with
        | Ir.I8 -> asm "movsx rax, al"
        | Ir.I16 -> asm "movsx rax, ax"
        | Ir.I32 -> asm "movsx rax, eax"
        | Ir.U8 -> asm "movzx rax, al"
        | Ir.U16 -> asm "movzx rax, ax"
        | Ir.U32 -> asm "mov eax, eax"
        | Ir.I64 | Ir.U64 | Ir.Ptr -> ()
        | Ir.F32 | Ir.F64 -> failwith "Shouldn't have float types in emit_int_binop"
        );

        result_t
    and emit_float_binop op e1 e2 =
        let rhs_t = emit_expr e2 in
        push_acc rhs_t;
        let lhs_t = emit_expr e1 in
        assert (lhs_t = rhs_t);
        pop rhs_t "xmm1";

        let op_suffix = match lhs_t with
            | Ir.F64 -> "sd"
            | Ir.F32 -> "ss"
            | _ -> failwith "Shouldn't have int types in emit_float_binop"
        in
        (match op with
        | Ir.Add -> asmf "add%s xmm0, xmm1" op_suffix
        | Ir.Sub -> asmf "sub%s xmm0, xmm1" op_suffix
        | Ir.Mul -> asmf "mul%s xmm0, xmm1" op_suffix
        | Ir.Div -> asmf "div%s xmm0, xmm1" op_suffix
        | Ir.CompEQ -> materialize_comparison lhs_t "e"
        | Ir.CompNEQ -> materialize_comparison lhs_t "ne"
        | Ir.CompLT -> materialize_comparison lhs_t "b"
        | Ir.CompLTE -> materialize_comparison lhs_t "be"
        | Ir.CompGT -> materialize_comparison lhs_t "a"
        | Ir.CompGTE -> materialize_comparison lhs_t "ae"
        | Ir.Rem | Ir.And | Ir.Or | Ir.Xor | Ir.ShiftLeft | Ir.ShiftRight -> failwith "Integer operations not allowed in emit_float_binop"
        );

        let result_t = match op with
            | Ir.CompEQ | Ir.CompNEQ | Ir.CompLT | Ir.CompLTE | Ir.CompGT | Ir.CompGTE -> Ir.I32
            | _ -> lhs_t
        in

        result_t
    and emit_expr expr =
        match expr with
        | Ir.ConstInt (typ, n) -> asmf "mov rax, %Ld" n; typ
        | Ir.ConstFloat (Ir.F64, n) -> asmf "movsd xmm0, [%s]" (address_of_constant_qword constants.qwords (Int64.bits_of_float n)); Ir.F64
        | Ir.ConstFloat (Ir.F32, n) -> asmf "movss xmm0, [%s]" (address_of_constant_dword constants.dwords (Int32.bits_of_float n)); Ir.F32
        | Ir.ConstFloat (_, _) -> failwith "Invalid type for ConstFloat"
        | Ir.ConstStringAddr s -> asmf "mov rax, __minicc_constant_string_%d" (id_of_string_lit constants.strings s); Ir.Ptr
        | Ir.LocalAddr local_id -> asmf "lea rax, [rbp + %d]" (find_local_offset local_id); Ir.Ptr
        | Ir.GlobalAddr v -> asmf "mov rax, %s" v; Ir.Ptr
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
                | Ir.F32 -> ld "movss" "xmm0" "dword"
                | Ir.F64 -> ld "movsd" "xmm0" "qword"
            );

            typ
        end            
        | Ir.BinOp (op, e1, e2) -> begin
            if Ir.is_integer (Ir.typecheck_expr e1)
            then emit_int_binop op e1 e2
            else emit_float_binop op e1 e2
        end
        | Ir.UnaryOp (op, e) -> begin
            match op with
            | Ir.Not -> let typ = emit_expr e in asm "not rax"; typ
            | Ir.Neg -> begin
                let typ = emit_expr e in
                (* For floats, we have to flip the sign bit rather than
                subtracting from zero so that negating zero gets negative
                zero instead of positive zero. *)
                (match typ with
                | Ir.F32 -> begin
                    asmf "movsd xmm1, [%s]" (address_of_constant_dword constants.dwords (Int32.shift_left 1l 31));
                    asm "xorpd xmm0, xmm1";
                end
                | Ir.F64 -> begin
                    asmf "movsd xmm1, [%s]" (address_of_constant_qword constants.qwords (Int64.shift_left 1L 63));
                    asm "xorpd xmm0, xmm1";
                end
                | _ -> asm "neg rax"
                );
                typ
            end
            | Ir.LogicalNot -> begin
                let typ = emit_expr e in
                (* Zero out xmm1 or rcx *)
                (match typ with
                | Ir.F32 | Ir.F64 -> asm "pxor xmm1, xmm1"
                | _ -> asm "xor rcx, rcx"
                );
                (* Compare rax/xmm0 to rcx/xmm1 *)
                materialize_comparison typ "e";
                Ir.I32
            end
        end
        | Ir.ConvertTo (to_t, e) -> begin
            let from_t = emit_expr e in
            (match from_t, to_t with
            | Ir.F32, Ir.F64 -> asm "cvtss2sd xmm0, xmm0"
            | Ir.F64, Ir.F32 -> asm "cvtsd2ss xmm0, xmm0"
            | Ir.F32, _ -> asm "cvtss2si rax, xmm0"
            | Ir.F64, _ -> asm "cvtsd2si rax, xmm0"
            | _, Ir.F32 -> asm "cvtsi2ss xmm0, rax"
            | _, Ir.F64 -> asm "cvtsi2sd xmm0, rax"
            | _, _ -> ()
            );

            to_t
        end
    in
    List.iter emit_inst ir_func.Ir.insts;
    (out_buffer, stack_bytes_allocated)

let emit ir_comp_unit =
    let constants = {
        strings = Hashtbl.create 100;
        dwords = Hashtbl.create 100;
        qwords = Hashtbl.create 100;
    } in
    let ob = Buffer.create 4096 in

    Buffer.add_string ob "default rel\n";

    List.iter (bprintf ob "extern %s\n") ir_comp_unit.Ir.extern_symbols;
    Buffer.add_string ob "section .bss\n";
    let sorted_globals = List.sort (fun (_, a) (_, b) -> Int.compare b.Ir.alignment a.Ir.alignment) ir_comp_unit.Ir.global_variables in
    List.iter (fun (v, storage) -> bprintf ob "align %d\n%s: resb %d\n" storage.Ir.alignment v storage.Ir.size) sorted_globals;

    Buffer.add_string ob "section .text\n";

    Hashtbl.iter (fun func_name func_ir -> begin
        let body_buf, stack_bytes_allocated = emit_func ir_comp_unit.Ir.func_table constants func_ir in

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
    ) ir_comp_unit.Ir.func_table;

    (* Output constants *)
    Buffer.add_string ob "section .rodata\n";
    
    (* qword constants *)
    Buffer.add_string ob "align 8\n";
    Hashtbl.iter (fun n () -> bprintf ob "__minicc_constant_qword_%Lu: dq %Lu\n" n n) constants.qwords;

    (* dword constants *)
    Buffer.add_string ob "align 4\n";
    Hashtbl.iter (fun n () -> bprintf ob "__minicc_constant_dword_%lu: dd %lu\n" n n) constants.dwords;

    (* String constants *)
    Hashtbl.iter (fun lit id ->
        bprintf ob "__minicc_constant_string_%d:\n" id;
        Buffer.add_string ob "db ";
        String.iter (fun c -> bprintf ob "%d, " (Char.code c)) lit;

        (* NUL terminate *)
        Buffer.add_string ob "0\n";
    ) constants.strings;

    (* Return buffer *)
    ob