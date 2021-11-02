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

let () =
    Arg.parse speclist handle_anon_arg usage_msg;
    if !runtests then
        Tests.run_all ()
    else
        match !input_filename_opt, !output_filename_opt with
        | Some input_filename, Some output_filename -> begin
            let in_chan = open_in input_filename in
            let out_chan = open_out output_filename in

            Lexing.from_channel in_chan |> Parser.compilation_unit Lexer.token |> Ast.to_ir |> Amd64.emit |> Buffer.output_buffer out_chan;
            
            close_in in_chan;
            close_out out_chan;
        end
        | _ -> failwith "Must provide both input and output filename"