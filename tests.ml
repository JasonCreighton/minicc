let should_not_compile msg fragment =
    let prog = Printf.sprintf "int main() { %s }" fragment in
    try
        Lexing.from_string prog |> Parser.compilation_unit Lexer.token |> Ast.build_func_table |> Amd64.emit |> ignore;
        failwith (Printf.sprintf "Expected compilation failure: %s" msg)
    with Ast.Compile_error _ -> ()

let run_all () =
    Ir.tests ();
    Ast.tests ();
    
    should_not_compile "Assignment to constant" "2 = 2;";
    should_not_compile "Assignment to undeclared variable" "int x; y = 7;";

    print_string "Tests passed.\n"