open Printf
open Ast

let rec put_in_rbx_rax e1 e2 =
	begin
		emit_expr e1;
		print_string "push rax\n";
		emit_expr e2;
		print_string "pop rbx\n";
	end

and emit_expr expr =
	match expr with
	| Lit n -> printf "mov rax, %d\n" n;
	| Add(e1, e2) -> (put_in_rbx_rax e1 e2; print_string "add rax, rbx\n")
	| Sub(e1, e2) -> (put_in_rbx_rax e1 e2; print_string "sub rbx, rax\nmov rax, rbx\n")
	| Mul(e1, e2) -> (put_in_rbx_rax e1 e2; print_string "imul rax, rbx\n")
	| Div(e1, e2) ->
		begin
			put_in_rbx_rax e1 e2;
			print_string "xor rdx, rdx\n"; (* FIXME: Should sign extend instead, I think? *)
			print_string "xchg rax, rbx\n";
			print_string "idiv rbx\n";
		end
	| Neg e1 -> (emit_expr e1; print_string "neg rax\n")

let emit expr =
	begin
		print_string "section .data\n";
		print_string "format:\n";
		print_string "db \"%d\", 10, 0\n";

		print_string "section .text\n";

		print_string "global main\n";
		print_string "extern printf\n";
		
		print_string "main:\n";
		print_string "push rbp\n";
		print_string "mov rbp, rsp\n";

		emit_expr expr;

		print_string "mov rdi, format\n";
		print_string "mov rsi, rax\n";
		print_string "call printf WRT ..plt\n";
		print_string "mov rax, 0\n";
		print_string "pop rbp\n";
		print_string "ret\n";

	end