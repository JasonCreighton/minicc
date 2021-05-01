{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let id    = alpha (alpha|digit)*
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | "char"         { CHAR }
  | "short"        { SHORT }
  | "int"          { INT }
  | "long"         { LONG }
  | "signed"       { SIGNED }
  | "unsigned"     { UNSIGNED }
  | "struct"       { STRUCT }
  | "if"           { IF }
  | "else"         { ELSE }
  | "while"        { WHILE }
  | id as lxm      { IDENTIFIER(lxm) }
  | digit+ as lxm { LITERAL_INT(int_of_string lxm) }  
  | '"'            { quoted_string [] lexbuf }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | '='            { EQUAL }
  | eof            { raise Eof }

and quoted_string pieces = parse
    '"'   { LITERAL_STRING(String.concat "" (List.rev pieces)) } (* End of quoted string*)
  | "\\r" { quoted_string ("\r" :: pieces) lexbuf }
  | "\\n" { quoted_string ("\n" :: pieces) lexbuf }
  | "\\t" { quoted_string ("\t" :: pieces) lexbuf }
  | "\\\\" { quoted_string ("\\" :: pieces) lexbuf }
  | "\\x" ((hex hex) as lxm) { quoted_string ((String.make 1 (char_of_int (Scanf.sscanf lxm "%x" (fun x -> x)))) :: pieces) lexbuf }
  | [^ '"' '\\']+ as lxm { quoted_string (lxm :: pieces) lexbuf }
