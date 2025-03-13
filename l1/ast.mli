
type var = string 

type oper = ADD | MUL | DIV | SUB | GTEQ | ASSIGN

type unary_oper = NEG | DEREF

type expr = 
       | Integer of int
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Seq of (expr list)
       | If_then_else of expr * expr * expr
       | While of expr * expr
       | Var of var

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
