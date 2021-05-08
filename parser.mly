%token <int> LITERAL_INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token VOID
%token CHAR SHORT INT LONG
%token FLOAT DOUBLE
%token SIGNED UNSIGNED
%token STRUCT
%token PLUS MINUS TIMES DIV
%token PLUSPLUS MINUSMINUS
%token EQUAL
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token IF
%token ELSE
%token WHILE
%token EOL

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

/* Precedence taken from here: https://en.cppreference.com/w/c/language/operator_precedence */
p0_expr
  : LITERAL_INT             { Ast.Lit $1 }
  | LITERAL_STRING          { Ast.LitString $1 }
  | IDENTIFIER              { Ast.VarRef $1 }
  | LPAREN expr RPAREN      { $2 }
;

p1_expr
  : p0_expr { $1 }
  | p1_expr PLUSPLUS { Ast.UnaryOp (Ast.PostInc, $1) }
  | p1_expr MINUSMINUS { Ast.UnaryOp (Ast.PostDec, $1) }
  | IDENTIFIER LPAREN argument_list RPAREN { Ast.Call ($1, List.rev $3) }
;

p2_expr
  : p1_expr { $1 }
  | PLUSPLUS p2_expr { Ast.UnaryOp (Ast.PreInc, $2) }
  | MINUSMINUS p2_expr { Ast.UnaryOp (Ast.PreDec, $2) }
  | MINUS p2_expr { Ast.UnaryOp (Ast.Neg, $2) }
;

p3_expr
  : p2_expr { $1 }
  | p3_expr TIMES p2_expr { Ast.BinOp (Ast.Mul, $1, $3) }
  | p3_expr DIV p2_expr { Ast.BinOp (Ast.Mul, $1, $3) }
;

p4_expr
  : p3_expr { $1 }
  | p4_expr PLUS p3_expr { Ast.BinOp (Ast.Add, $1, $3) }
  | p4_expr MINUS p3_expr { Ast.BinOp (Ast.Sub, $1, $3) }
;

p5_expr: p4_expr { $1 };
p6_expr: p5_expr { $1 };
p7_expr: p6_expr { $1 };
p8_expr: p7_expr { $1 };
p9_expr: p8_expr { $1 };
p10_expr: p9_expr { $1 };
p11_expr: p10_expr { $1 };
p12_expr: p11_expr { $1 };
p13_expr: p12_expr { $1 };

p14_expr
  : p13_expr { $1 }
  | p13_expr EQUAL p14_expr { Ast.Assign ($1, $3) }
;

p15_expr
  : p14_expr { $1 }
  | p15_expr COMMA p14_expr { Ast.Sequence ($1, $3) }
;

expr: p15_expr { $1 };

/* Note that we must use p14_expr in order to exclude the comma operator from the argument list */
argument_list:
    p14_expr { [$1] }
  | argument_list COMMA p14_expr { $3 :: $1 }
;