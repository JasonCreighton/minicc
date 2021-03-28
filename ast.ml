type node =
	| Lit of int
	| Add of node * node
	| Sub of node * node
	| Mul of node * node
	| Div of node * node
	| Neg of node

let rec eval expr =
	match expr with
	| Lit n -> n
	| Add(e1, e2) -> (eval e1) + (eval e2)
	| Sub(e1, e2) -> (eval e1) - (eval e2)
	| Mul(e1, e2) -> (eval e1) * (eval e2)
	| Div(e1, e2) -> (eval e1) / (eval e2)
	| Neg e1 -> -(eval e1)