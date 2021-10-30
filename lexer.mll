{
open Parser        (* The type token is defined in parser.mli *)

let integer_literal_flags flag_string = 
    let fs = String.lowercase_ascii flag_string in
    (if String.contains fs 'u' then Ast.IntLitFlags.unsigned else 0) lor
    (if String.contains fs 'l' then Ast.IntLitFlags.long else 0)

}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let id    = alpha (alpha|digit|'_')*
let intsuffix = ['u' 'U' 'l' 'L']*
rule token = parse
    [' ' '\t' '\r' '\n']     { token lexbuf }     (* skip blanks *)
  | "//" [^ '\n']* '\n' { token lexbuf } (* Skip single line comments *)
  | "/*"           { multiline_comment lexbuf }
  | "char"         { CHAR }
  | "short"        { SHORT }
  | "int"          { INT }
  | "long"         { LONG }
  | "signed"       { SIGNED }
  | "unsigned"     { UNSIGNED }
  | "void"         { VOID }
  | "struct"       { STRUCT }
  | "if"           { IF }
  | "else"         { ELSE }
  | "for"          { FOR }
  | "while"        { WHILE }
  | "return"       { RETURN }
  | "extern"       { EXTERN }
  | "const"        { CONST }
  | "..."          { ELLIPSIS }
  | "++"           { PLUSPLUS }
  | "--"           { MINUSMINUS }
  | "<<"           { LSHIFT }
  | ">>"           { RSHIFT }
  | '<'            { LESSTHAN }
  | "<="           { LESSTHANEQUAL }
  | '>'            { GREATERTHAN }
  | ">="           { GREATERTHANEQUAL }
  | "=="           { EQUALEQUAL }
  | "!="           { NOTEQUAL }
  | '&'            { AMPERSAND }
  | '^'            { BITXOR }
  | '|'            { BITOR }
  | '~'            { BITNOT }
  | '!'            { LOGICALNOT }
  | "&&"           { LOGICALAND }
  | "||"           { LOGICALOR }
  | '?'            { QUESTIONMARK }
  | ':'            { COLON }
  | '"'            { quoted_string [] lexbuf }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '%'            { REM }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { LBRACKET }
  | ']'            { RBRACKET }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | '='            { EQUAL }
  | id as lxm      { IDENTIFIER(lxm) }
  | ("0x" hex+ as lxm) (intsuffix as flags) { LITERAL_INT (Int64.of_string lxm, integer_literal_flags flags)}
  | (digit+ as lxm) (intsuffix as flags) { LITERAL_INT(Int64.of_string ("0u" ^ lxm), Ast.IntLitFlags.decimal lor (integer_literal_flags flags)) }
  | (digit+ '.' digit '*' ('e' digit+)?) as float_str ('f'?) as suffix {
    if suffix == "f"
    then LITERAL_FLOAT (Int32.float_of_bits (Int32.bits_of_float (Float.of_string float_str)))
    else LITERAL_DOUBLE (Float.of_string float_str)
  }
  | eof            { EOF }

(* Multiline comments need special handling to avoid consuming too much input
or interacting incorrectly with strings *)
and multiline_comment = parse
    "*/"     { token lexbuf }             (* End comment *)
  | [^ '*']+ { multiline_comment lexbuf } (* Skip non-star charcters *)
  | '*'      { multiline_comment lexbuf } (* Skip single star *)

and quoted_string pieces = parse
    '"'   { LITERAL_STRING(String.concat "" (List.rev pieces)) } (* End of quoted string*)
  | "\\r" { quoted_string ("\r" :: pieces) lexbuf }
  | "\\n" { quoted_string ("\n" :: pieces) lexbuf }
  | "\\t" { quoted_string ("\t" :: pieces) lexbuf }
  | "\\\\" { quoted_string ("\\" :: pieces) lexbuf }
  | "\\x" ((hex hex) as lxm) { quoted_string ((String.make 1 (char_of_int (Scanf.sscanf lxm "%x" (fun x -> x)))) :: pieces) lexbuf }
  | [^ '"' '\\']+ as lxm { quoted_string (lxm :: pieces) lexbuf }
