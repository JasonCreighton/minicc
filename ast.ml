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
and expr =
	| Lit of int
	| LitString of string
	| Assign of expr * expr
	| VarRef of string
	| Add of expr * expr
	| Sub of expr * expr
	| Mul of expr * expr
	| Div of expr * expr
	| Neg of expr
	| Call of string * expr list