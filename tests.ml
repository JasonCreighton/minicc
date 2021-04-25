let should_not_compile fragment =
	let prog = Printf.sprintf "int main() { %s }" fragment in
	try
		(* FIXME: Don't use stdout here, emit to a buffer or something *)
		Lexing.from_string prog |> Parser.decl Lexer.token |> Amd64.emit stdout;
		failwith "Compile error expected"
	with Amd64.Compile_error _ -> ()

let run_all () =
	should_not_compile "2 = 2;";
	should_not_compile "int x; y = 7;";

	print_string "Tests passed.\n"