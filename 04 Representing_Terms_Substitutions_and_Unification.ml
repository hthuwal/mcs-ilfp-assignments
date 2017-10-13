type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * term list;;
type arity = int;;
type signature = (symbol * arity) list;;

