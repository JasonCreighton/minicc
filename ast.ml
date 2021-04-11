type decl =
	| Function of string * stmt
and stmt =
    | CompoundStmt of stmt list
	| ExprStmt of expr
and expr =
	| Lit of int
	| LitString of string
	| Add of expr * expr
	| Sub of expr * expr
	| Mul of expr * expr
	| Div of expr * expr
	| Neg of expr
	| Call of string * expr list