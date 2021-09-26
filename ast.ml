type int_size =
    | Char
    | Short
    | Int
    | Long

type ctype =
    | Void
    | Signed of int_size
    | Unsigned of int_size
    | Float
    | Double
    | PointerTo of ctype
    | ArrayOf of ctype * int

type binop =
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

type unaryop =
    | PreInc
    | PreDec
    | PostInc
    | PostDec
    | LogicalNot
    | BitNot
    | Neg
    | AddressOf

type decl =
    | Function of ctype * string * (ctype * string) list * stmt
    | FunctionDecl of ctype * string * (ctype * string) list
and stmt =
    | CompoundStmt of stmt list
    | ExprStmt of expr
    | IfElseStmt of expr * stmt * stmt
    | DeclVar of ctype * string
    | DeclAssign of ctype * string * expr
    | WhileStmt of expr * stmt
    | ForStmt of expr * expr * expr * stmt
    | ReturnStmt of expr option
and expr =
    | Lit of int
    | LitString of string
    | Assign of expr * expr
    | VarRef of string
    | Subscript of expr * expr
    | Deref of expr
    | BinOp of binop * expr * expr
    | UnaryOp of unaryop * expr
    | Conditional of expr * expr * expr (* "ternary" operator *)
    | Sequence of expr * expr (* "comma" operator *)
    (* Technically logical AND/OR are binary operators, but they are special because
     of short circuiting and will have different code gen, so we separate them *)
    | LogicalAnd of expr * expr
    | LogicalOr of expr * expr 
    | Call of string * expr list