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

let emit_expr expr =
	let lit_table = Hashtbl.create 100 in
	let rec put_in_rax_rbx e1 e2 =
		begin
			(*
			Note: Evaluate RHS first, so we can end up with the LHS in rax.
			This makes things nicer for subtraction and division.
			*)
			emit' e2;
			print_string "push rax\n";
			emit' e1;
			print_string "pop rbx\n";
		end
	and emit' expr =
		match expr with
		| Lit n -> printf "mov rax, %d\n" n;
		| LitString s -> printf "mov rax, string_lit_%d\n" (id_of_string_lit lit_table s)
		| Add(e1, e2) -> (put_in_rax_rbx e1 e2; print_string "add rax, rbx\n")
		| Sub(e1, e2) -> (put_in_rax_rbx e1 e2; print_string "sub rax, rbx\n")
		| Mul(e1, e2) -> (put_in_rax_rbx e1 e2; print_string "imul rax, rbx\n")
		| Div(e1, e2) ->
			begin
				put_in_rax_rbx e1 e2;
				print_string "cqo\n";
				print_string "idiv rbx\n";
			end
		| Neg e1 -> (emit' e1; print_string "neg rax\n")
		| Call(func_name, args) ->
			begin
				(* Evaluate arguments onto stack *)
				List.iter (fun a -> emit' a; print_string "push rax\n") (List.rev args);

				(* Pop stack values into calling convention registers *)
				List.iteri (fun i reg -> if i < (List.length args) then printf "pop %s\n" reg else ()) call_registers;

				(* FIXME: We will have to distinguish between library calls and user-defined calls because of the PLT *)
				printf "call %s WRT ..plt\n" func_name;
			end
	in
	emit' expr; 
	(* Return string literal table *)
	lit_table

let emit expr =
	begin
		print_string "section .text\n";

		print_string "global main\n";
		print_string "extern printf\n";
		
		print_string "main:\n";
		print_string "push rbp\n";
		print_string "mov rbp, rsp\n";

		let lit_table = emit_expr expr in

		print_string "mov rax, 0\n";
		print_string "pop rbp\n";
		print_string "ret\n";

		(* Output string literals *)
		print_string "section .data\n";
		Hashtbl.iter (fun lit id ->
			printf "string_lit_%d:\n" id;
			print_string "db ";
			String.iter (fun c -> printf "%d, " (Char.code c)) lit;

			(* NUL terminate *)
			print_string "0\n";
		) lit_table;
	end