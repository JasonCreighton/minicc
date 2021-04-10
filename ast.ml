type decl =
	| Function of string * node
and node =
	| Lit of int
	| LitString of string
	| Add of node * node
	| Sub of node * node
	| Mul of node * node
	| Div of node * node
	| Neg of node
	| Call of string * node list