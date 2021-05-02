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
        Lexing.from_channel stdin |> Parser.decl Lexer.token |> Amd64.emit stdout;
        flush stdout