%token <int> LITERAL_INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token VOID
%token CHAR SHORT INT LONG
%token FLOAT DOUBLE
%token SIGNED UNSIGNED
%token STRUCT
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
  ctype IDENTIFIER LPAREN RPAREN compound_statement { Ast.Function ($2, $5) }
;

ctype
  : VOID { Ast.Void }
  | int_size { Ast.Signed $1 }
  | SIGNED int_size { Ast.Signed $2 }
  | UNSIGNED int_size { Ast.Unsigned $2 }
  | FLOAT { Ast.Float }
  | DOUBLE { Ast.Double }
;

int_size
  : CHAR { Ast.Char }
  | SHORT { Ast.Short }
  | INT { Ast.Int }
  | LONG { Ast.Long }
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
  | ctype IDENTIFIER SEMICOLON { Ast.DeclVar ($1, $2) }
  | ctype IDENTIFIER EQUAL expr SEMICOLON { Ast.DeclAssign ($1, $2, $4) }  
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
  | expr PLUS expr          { Ast.BinOp (Ast.Add, $1, $3) }
  | expr MINUS expr         { Ast.BinOp (Ast.Sub, $1, $3) }
  | expr TIMES expr         { Ast.BinOp (Ast.Mul, $1, $3) }
  | expr DIV expr           { Ast.BinOp (Ast.Div, $1, $3) }
  | MINUS expr %prec UMINUS { Ast.UnaryOp (Ast.Neg, $2) }
  | IDENTIFIER LPAREN RPAREN { Ast.Call ($1, []) }
  | IDENTIFIER LPAREN argument_list RPAREN { Ast.Call ($1, List.rev $3) }
;

argument_list:
    expr { [$1] }
  | argument_list COMMA expr { $3 :: $1 }
;