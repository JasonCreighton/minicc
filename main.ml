let usage_msg = "minicc [-runtests] [-i <input file> -o <output file>]"
let runtests = ref false
let input_filename_opt = ref None
let output_filename_opt = ref None

let handle_anon_arg _ = ()

let speclist = [
    ("-runtests", Arg.Set runtests, "Run tests");
    ("-i", Arg.String (fun s -> input_filename_opt := Some s), "Input filename");
    ("-o", Arg.String (fun s -> output_filename_opt := Some s), "Output filename");
]

let time_ms f = begin
    let start = Unix.gettimeofday () in
    let result = f () in
    let elapsed = Unix.gettimeofday () -. start in
    (result, elapsed *. 1000.0)
end

let () =
    Arg.parse speclist handle_anon_arg usage_msg;
    if !runtests then
        Tests.run_all ()
    else
        match !input_filename_opt, !output_filename_opt with
        | Some input_filename, Some output_filename -> begin
            let in_chan = open_in input_filename in
            let out_chan = open_out output_filename in

            let ast, parse_time_ms = time_ms (fun () -> Parser.compilation_unit Lexer.token (Lexing.from_channel in_chan)) in
            let ir, ast_to_ir_time_ms = time_ms (fun () -> Ast.to_ir ast) in
            let buf, amd64_emit_time_ms = time_ms (fun () -> Amd64.emit ir) in

            Buffer.output_buffer out_chan buf;

            Printf.printf "Parsing: %.1f ms\nAST -> IR: %.1f ms\nIR -> AMD64: %.1f ms\n" parse_time_ms ast_to_ir_time_ms amd64_emit_time_ms;

            close_in in_chan;
            close_out out_chan;
        end
        | _ -> failwith "Must provide both input and output filename"