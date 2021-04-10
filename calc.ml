let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.decl Lexer.token lexbuf in
        (* print_int (Ast.eval result); print_newline(); flush stdout; *)
        Amd64.emit stdout result; flush stdout;
    done
  with Lexer.Eof ->
    exit 0