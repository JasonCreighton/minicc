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

let emit_func body =
	let out_buffer = Buffer.create 4096 in
	let lit_table = Hashtbl.create 100 in
	let var_table = Hashtbl.create 100 in
	let num_stack_locs = ref 0 in
	let next_label_id = ref 0 in
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
	and find_var_loc v =
		match Hashtbl.find_opt var_table v with
		| Some loc -> loc
		| None -> raise (Compile_error (sprintf "Undeclared variable '%s'" v))
	and asm str = begin
		Buffer.add_char out_buffer '\t';
		Buffer.add_string out_buffer str;
		Buffer.add_char out_buffer '\n';
	end
	and asm_raw str = Buffer.add_string out_buffer str
	and decl_var v =
		num_stack_locs := !num_stack_locs + 1;
		Hashtbl.add var_table v (!num_stack_locs * 8)
	and assign_var v expr =
		emit_expr expr;
		ksprintf asm "mov [rbp - %d], rax" (find_var_loc v)
	and emit_stmt stmt =
		match stmt with
		| DeclVar v -> decl_var v
		| DeclAssign (v, expr) -> decl_var v; assign_var v expr
		| ExprStmt expr -> emit_expr expr
		| CompoundStmt stmts -> List.iter emit_stmt stmts
		| IfElseStmt (cond, then_stmt, else_stmt) -> begin
			let else_label_id = !next_label_id in
			next_label_id := !next_label_id + 1;
			emit_expr cond;
			asm "cmp rax, 0";
			ksprintf asm "je .label_%d" else_label_id;
			emit_stmt then_stmt;
			ksprintf asm_raw ".label_%d:\n" else_label_id;
			emit_stmt else_stmt;
		end
	and emit_expr expr =
		match expr with
		| Lit n -> ksprintf asm "mov rax, %d" n;
		| LitString s -> ksprintf asm "mov rax, string_lit_%d" (id_of_string_lit lit_table s)
		| Assign (VarRef v, rhs) -> assign_var v rhs
		| Assign (_, _) -> raise (Compile_error "Assignment to non-lvalue")
		| VarRef v -> ksprintf asm "mov rax, [rbp - %d]" (find_var_loc v)
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
				List.iteri (fun i reg -> if i < (List.length args) then ksprintf asm "pop %s" reg else ()) call_registers;

				(* FIXME: We will have to distinguish between library calls and user-defined calls because of the PLT *)
				ksprintf asm "call %s WRT ..plt" func_name;
			end
	in
	emit_stmt body; 
	(out_buffer, lit_table, !num_stack_locs)

let emit oc decl =
	match decl with
	| Function (func_name, func_body) ->
		begin
			let body_buf, lit_table, num_stack_locs = emit_func func_body in

			output_string oc "section .text\n";

			fprintf oc "global %s\n" func_name;
			output_string oc "extern printf\n";
			
			fprintf oc "%s:\n" func_name;
			output_string oc "\tpush rbp\n";
			output_string oc "\tmov rbp, rsp\n";

			(* Stack needs to be 16 byte aligned *)
			let eff_num_stack_locs = ((num_stack_locs + 1) / 2) * 2 in
			fprintf oc "\tsub rsp, %d\n" (eff_num_stack_locs * 8);

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