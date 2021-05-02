let should_not_compile msg fragment =
    let prog = Printf.sprintf "int main() { %s }" fragment in
    try
        (* FIXME: Don't use stdout here, emit to a buffer or something *)
        Lexing.from_string prog |> Parser.decl Lexer.token |> Amd64.emit stdout;
        failwith (Printf.sprintf "Expected compilation failure: %s" msg)
    with Amd64.Compile_error _ -> ()

let run_all () =
    should_not_compile "Assignment to constant" "2 = 2;";
    should_not_compile "Assignment to undeclared variable" "int x; y = 7;";

    print_string "Tests passed.\n"