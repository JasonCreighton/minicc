{
open Parser        (* The type token is defined in parser.mli *)
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let id    = alpha (alpha|digit|'_')*
rule token = parse
    [' ' '\t' '\r' '\n']     { token lexbuf }     (* skip blanks *)
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
  | digit+ as lxm { LITERAL_INT(Int64.of_string lxm) }
  | eof            { EOF }

and quoted_string pieces = parse
    '"'   { LITERAL_STRING(String.concat "" (List.rev pieces)) } (* End of quoted string*)
  | "\\r" { quoted_string ("\r" :: pieces) lexbuf }
  | "\\n" { quoted_string ("\n" :: pieces) lexbuf }
  | "\\t" { quoted_string ("\t" :: pieces) lexbuf }
  | "\\\\" { quoted_string ("\\" :: pieces) lexbuf }
  | "\\x" ((hex hex) as lxm) { quoted_string ((String.make 1 (char_of_int (Scanf.sscanf lxm "%x" (fun x -> x)))) :: pieces) lexbuf }
  | [^ '"' '\\']+ as lxm { quoted_string (lxm :: pieces) lexbuf }
