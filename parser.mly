%token <int> LITERAL_INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token TYPE_INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start decl             /* the entry point */
%type <Ast.decl> decl
%%
;
/* Total hack for now to get a minimal function parsing */
decl:
	TYPE_INT IDENTIFIER LPAREN RPAREN compound_statement { Ast.Function ($2, $5) }
;

compound_statement:
    LBRACE RBRACE { Ast.CompoundStmt [] }
  | LBRACE statement_list RBRACE { Ast.CompoundStmt (List.rev $2) }
;

statement_list:
    statement { [$1] }
  | statement_list statement { $2 :: $1 }
;

statement:
	expr SEMICOLON { Ast.ExprStmt $1 }
;

expr:
    LITERAL_INT             { Ast.Lit $1 }
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