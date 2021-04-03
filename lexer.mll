{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id    = alpha (alpha|digit)*
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | id as lxm      { IDENTIFIER(lxm) }
  | digit+ as lxm { INT(int_of_string lxm) }
  | '"' ([^ '"']* as str) '"' { LITERAL_STRING(str) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ','            { COMMA }
  | eof            { raise Eof }
