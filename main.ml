let usage_msg = "minicc [-runtests]"
let runtests = ref false

let handle_anon_arg _ = ()

let speclist =
	[("-runtests", Arg.Set runtests, "Run tests")]

let () =
	Arg.parse speclist handle_anon_arg usage_msg;
	if !runtests then
		Tests.run_all ()
	else	
		try
			let lexbuf = Lexing.from_channel stdin in
			while true do
				let result = Parser.decl Lexer.token lexbuf in
				Amd64.emit stdout result;
				flush stdout;
			done
		with Lexer.Eof ->
			exit 0