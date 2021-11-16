let should_not_compile msg fragment =
    let prog = Printf.sprintf "int main() { %s }" fragment in
    try
        Lexing.from_string prog |> Parser.compilation_unit Lexer.token |> Ast.to_ir |> Amd64.emit |> ignore;
        failwith (Printf.sprintf "Expected compilation failure: %s" msg)
    with Ast.Compile_error _ -> ()

let run_all () =
    Liveness.tests ();
    Regalloc.tests ();
    Ir.tests ();
    Ast.tests ();
    
    should_not_compile "Assignment to constant" "2 = 2;";
    should_not_compile "Assignment to undeclared variable" "int x; y = 7;";
    should_not_compile "Reference undeclared variable" "x;";
    should_not_compile "Declare same variable twice in same scope" "int x; int x;";

    print_string "Tests passed.\n"