%token <int> INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token COMMA
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Ast.node> main
%type <Ast.node list> argument_list
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { Ast.Lit $1 }
  | LITERAL_STRING          { Ast.LitString $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.Add ($1, $3) }
  | expr MINUS expr         { Ast.Sub ($1, $3) }
  | expr TIMES expr         { Ast.Mul ($1, $3) }
  | expr DIV expr           { Ast.Div ($1, $3) }
  | MINUS expr %prec UMINUS { Ast.Neg $2 }
  | IDENTIFIER LPAREN RPAREN { Ast.Call ($1, []) }
  | IDENTIFIER LPAREN argument_list RPAREN { Ast.Call ($1, List.rev $3) }
;

argument_list:
    expr { [$1] }
  | argument_list COMMA expr { $3 :: $1 }
;