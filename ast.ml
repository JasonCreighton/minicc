type decl =
	| Function of string * stmt
and stmt =
    | CompoundStmt of stmt list
	| ExprStmt of expr
	| IfStmt of expr * stmt
	| IfElseStmt of expr * stmt * stmt
and expr =
	| Lit of int
	| LitString of string
	| Add of expr * expr
	| Sub of expr * expr
	| Mul of expr * expr
	| Div of expr * expr
	| Neg of expr
	| Call of string * expr list