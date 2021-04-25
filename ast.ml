type decl =
	| Function of string * stmt
and stmt =
    | CompoundStmt of stmt list
	| ExprStmt of expr
	| IfElseStmt of expr * stmt * stmt
	| DeclVar of string
	| DeclAssign of string * expr
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