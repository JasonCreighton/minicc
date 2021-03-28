%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Ast.node> main
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { Ast.Lit $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.Add ($1, $3) }
  | expr MINUS expr         { Ast.Sub ($1, $3) }
  | expr TIMES expr         { Ast.Mul ($1, $3) }
  | expr DIV expr           { Ast.Div ($1, $3) }
  | MINUS expr %prec UMINUS { Ast.Neg $2 }
;