type decl =
    | Function of string * stmt
and int_size =
    | Char
    | Short
    | Int
    | Long
and ctype =
    | Void
    | Signed of int_size
    | Unsigned of int_size
    | Float
    | Double
and stmt =
    | CompoundStmt of stmt list
    | ExprStmt of expr
    | IfElseStmt of expr * stmt * stmt
    | DeclVar of ctype * string
    | DeclAssign of ctype * string * expr
    | WhileStmt of expr * stmt
and binop =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | BitAnd
    | BitXor
    | BitOr
    | BitShiftLeft
    | BitShiftRight
    | CompEQ
    | CompNEQ
    | CompLT
    | CompGT
    | CompLTE
    | CompGTE
and unaryop =
    | PreInc
    | PreDec
    | PostInc
    | PostDec
    | LogicalNot
    | BitNot
    | Neg
and expr =
    | Lit of int
    | LitString of string
    | Assign of expr * expr
    | VarRef of string
    | BinOp of binop * expr * expr
    | UnaryOp of unaryop * expr
    | Conditional of expr * expr * expr (* "ternary" operator *)
    | Sequence of expr * expr (* "comma" operator *)
    (* Technically logical AND/OR are binary operators, but they are special because
     of short circuiting and will have different code gen, so we separate them *)
    | LogicalAnd of expr * expr
    | LogicalOr of expr * expr 
    | Call of string * expr list