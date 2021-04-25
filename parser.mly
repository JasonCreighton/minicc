%token <int> LITERAL_INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token TYPE_INT
%token PLUS MINUS TIMES DIV
%token EQUAL
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token IF
%token ELSE
%token WHILE
%token EOL


/* Lower precedence is listed first */
%right EQUAL
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

/* Hack to resolve "dangling else" shift/reduce conflict */
%nonassoc RPAREN
%nonassoc ELSE

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

statement
  : expr SEMICOLON { Ast.ExprStmt $1 }
  | compound_statement { $1 }
  | TYPE_INT IDENTIFIER SEMICOLON { Ast.DeclVar $2 }
  | TYPE_INT IDENTIFIER EQUAL expr SEMICOLON { Ast.DeclAssign ($2, $4) }  
  | IF LPAREN expr RPAREN statement { Ast.IfElseStmt ($3, $5, Ast.CompoundStmt []) }
  | IF LPAREN expr RPAREN statement ELSE statement { Ast.IfElseStmt ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN statement { Ast.WhileStmt ($3, $5) }
;

expr:
    LITERAL_INT             { Ast.Lit $1 }
  | LITERAL_STRING          { Ast.LitString $1 }
  | IDENTIFIER              { Ast.VarRef $1 }
  | expr EQUAL expr         { Ast.Assign ($1, $3) }
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