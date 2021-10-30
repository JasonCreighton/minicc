%token <int64 * Ast.IntLitFlags.t> LITERAL_INT
%token <string> IDENTIFIER
%token <string> LITERAL_STRING
%token <float> LITERAL_DOUBLE
%token <float> LITERAL_FLOAT
%token VOID
%token CONST
%token CHAR SHORT INT LONG
%token FLOAT DOUBLE
%token SIGNED UNSIGNED
%token STRUCT
%token PLUS MINUS TIMES DIV REM
%token PLUSPLUS MINUSMINUS
%token LSHIFT RSHIFT
%token LESSTHAN LESSTHANEQUAL GREATERTHAN GREATERTHANEQUAL EQUALEQUAL NOTEQUAL
%token AMPERSAND
%token BITXOR BITOR BITNOT
%token LOGICALAND
%token LOGICALOR
%token LOGICALNOT
%token ADDRESSOF
%token QUESTIONMARK
%token COLON
%token EQUAL
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token IF
%token ELSE
%token FOR
%token WHILE
%token RETURN
%token EXTERN
%token ELLIPSIS
%token EOF

/* Hack to resolve "dangling else" shift/reduce conflict */
%nonassoc RPAREN
%nonassoc ELSE

%start compilation_unit             /* the entry point */
%type <Ast.decl list> compilation_unit
%%

compilation_unit : decl_list EOF { List.rev $1 };

decl_list
  : decl { [$1] }
  | decl_list decl { $2 :: $1 }
;

decl
  : ctype IDENTIFIER function_parameters compound_statement { Ast.Function ($1, $2, List.rev $3, $4) }
  | EXTERN ctype IDENTIFIER function_parameters SEMICOLON { Ast.FunctionDecl ($2, $3, List.rev $4) }
;

function_parameters
  : LPAREN RPAREN { [] }
  | LPAREN function_parameter_list RPAREN { $2 }
;

function_parameter_list
  : named_ctype { [$1] }
  | function_parameter_list COMMA named_ctype { $3 :: $1 }
  | function_parameter_list COMMA ELLIPSIS { $1 } /* Ignore varargs for now */
;

named_ctype
  : ctype IDENTIFIER { ($1, $2) }
  | ctype prefix_ctype_modifiers IDENTIFIER { ($1 |> $2, $3) }
  | ctype IDENTIFIER postfix_ctype_modifiers { ($1 |> $3, $2) }
  /* We apply the prefix modifier first. (eg, "int *x[5]" is an array of pointers to int, not a pointer to an array of ints) */
  | ctype prefix_ctype_modifiers IDENTIFIER postfix_ctype_modifiers { ($1 |> $2 |> $4, $3) }
;

ctype
  : primitive_type { $1 }
  | CONST primitive_type { $2 } /* Ignore const for now */
;

/* Kind of odd productions, they return a function that should be applied to
   the ctype in order to yield the modified ctype. */
prefix_ctype_modifiers
  : TIMES { fun typ -> Ast.PointerTo typ }
  | TIMES prefix_ctype_modifiers { fun typ -> Ast.PointerTo ($2 typ) }
;

postfix_ctype_modifiers
  : LBRACKET LITERAL_INT RBRACKET { fun typ -> Ast.ArrayOf (typ, Int64.to_int (fst $2)) }
  | LBRACKET LITERAL_INT RBRACKET postfix_ctype_modifiers { fun typ -> Ast.ArrayOf ($4 typ, Int64.to_int (fst $2)) }
;

primitive_type
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
  | named_ctype SEMICOLON { let (name, typ) = $1 in Ast.DeclVar (name, typ) }
  | ctype IDENTIFIER EQUAL expr SEMICOLON { Ast.DeclAssign ($1, $2, $4) }  
  | IF LPAREN expr RPAREN statement { Ast.IfElseStmt ($3, $5, Ast.CompoundStmt []) }
  | IF LPAREN expr RPAREN statement ELSE statement { Ast.IfElseStmt ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN statement { Ast.WhileStmt ($3, $5) }
  | FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN statement { Ast.ForStmt ($3, $5, $7, $9) }
  | RETURN SEMICOLON { Ast.ReturnStmt None }
  | RETURN expr SEMICOLON { Ast.ReturnStmt (Some $2) }
;

/* Precedence taken from here: https://en.cppreference.com/w/c/language/operator_precedence */
p0_expr
  : LITERAL_INT             { let n, flags = $1 in Ast.Lit (n, flags) }
  | LITERAL_DOUBLE          { Ast.LitDouble $1 }
  | LITERAL_FLOAT           { Ast.LitFloat $1 }
  | LITERAL_STRING          { Ast.LitString $1 }
  | IDENTIFIER              { Ast.VarRef $1 }
  | LPAREN expr RPAREN      { $2 }
;

p1_expr
  : p0_expr { $1 }
  | p1_expr PLUSPLUS { Ast.UnaryOp (Ast.PostInc, $1) }
  | p1_expr MINUSMINUS { Ast.UnaryOp (Ast.PostDec, $1) }
  | IDENTIFIER LPAREN argument_list RPAREN { Ast.Call ($1, List.rev $3) }
  | IDENTIFIER LPAREN RPAREN { Ast.Call ($1, []) }
  | p1_expr LBRACKET expr RBRACKET { Ast.Subscript ($1, $3) }
;

p2_expr
  : p1_expr { $1 }
  | BITNOT p2_expr { Ast.UnaryOp (Ast.BitNot, $2) }
  | LOGICALNOT p2_expr { Ast.UnaryOp (Ast.LogicalNot, $2) }
  | TIMES p2_expr { Ast.Deref $2 }
  | AMPERSAND p2_expr { Ast.UnaryOp (Ast.AddressOf, $2) }
  | PLUSPLUS p2_expr { Ast.UnaryOp (Ast.PreInc, $2) }
  | MINUSMINUS p2_expr { Ast.UnaryOp (Ast.PreDec, $2) }
  | MINUS p2_expr { Ast.UnaryOp (Ast.Neg, $2) }
;

p3_expr
  : p2_expr { $1 }
  | p3_expr TIMES p2_expr { Ast.BinOp (Ast.Mul, $1, $3) }
  | p3_expr DIV p2_expr { Ast.BinOp (Ast.Div, $1, $3) }
  | p3_expr REM p2_expr { Ast.BinOp (Ast.Rem, $1, $3) }
;

p4_expr
  : p3_expr { $1 }
  | p4_expr PLUS p3_expr { Ast.BinOp (Ast.Add, $1, $3) }
  | p4_expr MINUS p3_expr { Ast.BinOp (Ast.Sub, $1, $3) }
;

p5_expr
  : p4_expr { $1 }
  | p5_expr LSHIFT p4_expr { Ast.BinOp (Ast.BitShiftLeft, $1, $3) }
  | p5_expr RSHIFT p4_expr { Ast.BinOp (Ast.BitShiftRight, $1, $3) }
;

p6_expr
  : p5_expr { $1 }
  | p6_expr LESSTHAN p5_expr { Ast.BinOp (Ast.CompLT, $1, $3) }
  | p6_expr LESSTHANEQUAL p5_expr { Ast.BinOp (Ast.CompLTE, $1, $3) }
  | p6_expr GREATERTHAN p5_expr { Ast.BinOp (Ast.CompGT, $1, $3) }
  | p6_expr GREATERTHANEQUAL p5_expr { Ast.BinOp (Ast.CompGTE, $1, $3) }
;

p7_expr
  : p6_expr { $1 }
  | p7_expr EQUALEQUAL p6_expr { Ast.BinOp (Ast.CompEQ, $1, $3) }
  | p7_expr NOTEQUAL p6_expr { Ast.BinOp (Ast.CompNEQ, $1, $3) }
;

p8_expr : p7_expr { $1 } | p8_expr AMPERSAND p7_expr { Ast.BinOp (Ast.BitAnd, $1, $3) };
p9_expr : p8_expr { $1 } | p9_expr BITXOR p8_expr { Ast.BinOp (Ast.BitXor, $1, $3) };
p10_expr : p9_expr { $1 } | p10_expr BITOR  p9_expr { Ast.BinOp (Ast.BitOr, $1, $3) };
p11_expr : p10_expr { $1 } | p11_expr LOGICALAND p10_expr { Ast.LogicalAnd ($1, $3) };
p12_expr : p11_expr { $1 } | p12_expr LOGICALOR p11_expr { Ast.LogicalOr ($1, $3) };
p13_expr : p12_expr { $1 } | p12_expr QUESTIONMARK expr COLON p13_expr { Ast.Conditional ($1, $3, $5) };

/* Note that the LHS of assignment is restricted to precedence 2 or higher operators */
p14_expr
  : p13_expr { $1 }
  | p2_expr EQUAL p14_expr { Ast.Assign ($1, $3) }
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