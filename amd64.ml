open Printf
open Ast

let call_registers = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"]

let id_of_string_lit lit_table s =
	match Hashtbl.find_opt lit_table s with
	| Some id -> id
	| None ->
		let new_id = Hashtbl.length lit_table in
		Hashtbl.add lit_table s new_id;
		new_id

let emit_expr oc expr =
	let lit_table = Hashtbl.create 100 in
	let rec put_in_rax_rbx e1 e2 =
		begin
			(*
			Note: Evaluate RHS first, so we can end up with the LHS in rax.
			This makes things nicer for subtraction and division.
			*)
			emit' e2;
			output_string oc "push rax\n";
			emit' e1;
			output_string oc "pop rbx\n";
		end
	and emit' expr =
		match expr with
		| Lit n -> fprintf oc "mov rax, %d\n" n;
		| LitString s -> fprintf oc "mov rax, string_lit_%d\n" (id_of_string_lit lit_table s)
		| Add(e1, e2) -> (put_in_rax_rbx e1 e2; output_string oc "add rax, rbx\n")
		| Sub(e1, e2) -> (put_in_rax_rbx e1 e2; output_string oc "sub rax, rbx\n")
		| Mul(e1, e2) -> (put_in_rax_rbx e1 e2; output_string oc "imul rax, rbx\n")
		| Div(e1, e2) ->
			begin
				put_in_rax_rbx e1 e2;
				output_string oc "cqo\n";
				output_string oc "idiv rbx\n";
			end
		| Neg e1 -> (emit' e1; output_string oc "neg rax\n")
		| Call(func_name, args) ->
			begin
				(* Evaluate arguments onto stack *)
				List.iter (fun a -> emit' a; output_string oc "push rax\n") (List.rev args);

				(* Pop stack values into calling convention registers *)
				List.iteri (fun i reg -> if i < (List.length args) then fprintf oc "pop %s\n" reg else ()) call_registers;

				(* FIXME: We will have to distinguish between library calls and user-defined calls because of the PLT *)
				fprintf oc "call %s WRT ..plt\n" func_name;
			end
	in
	emit' expr; 
	(* Return string literal table *)
	lit_table

let emit oc decl =
	match decl with
	| Function (func_name, expr) ->
		begin
			output_string oc "section .text\n";

			fprintf oc "global %s\n" func_name;
			output_string oc "extern printf\n";
			
			fprintf oc "%s:\n" func_name;
			output_string oc "push rbp\n";
			output_string oc "mov rbp, rsp\n";

			let lit_table = emit_expr oc expr in

			output_string oc "mov rax, 0\n";
			output_string oc "pop rbp\n";
			output_string oc "ret\n";

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