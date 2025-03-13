(* using the structure from the Slang interpreter *)

open Past 

let complain = Errors.complain

let internal_error msg = complain ("INTERNAL ERROR: " ^ msg) 

let report_expecting e msg t = 
    let loc = loc_of_expr e in 
    let loc_str = string_of_loc loc in 
    let e_str = string_of_expr e in 
    let t_str = string_of_type t in 
    complain ("ERROR at location " ^ 
	      loc_str ^ "\nExpression " ^ e_str ^ 
	      "\nhas type " ^ t_str ^ ", but expecting " ^ msg) 

let report_types_not_equal loc t1 t2 = 
    let loc_str = string_of_loc loc in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("Error near location " ^ loc_str ^ 
              "\nExpecting type " ^ t1_str ^ " to be equal to type " ^ t2_str)

let report_type_mismatch (e1, t1) (e2, t2) = 
    let loc1 = loc_of_expr e1 in 
    let loc2 = loc_of_expr e2 in 
    let loc1_str = string_of_loc loc1 in 
    let loc2_str = string_of_loc loc2 in 
    let e1_str = string_of_expr e1 in 
    let e2_str = string_of_expr e2 in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("ERROR, Type Mismatch: expecting equal types, however\n" ^ 
	      "at location " ^ loc1_str ^ "\nexpression " ^ e1_str ^ "\nhas type " ^ t1_str ^ 
	      " and at location " ^ loc2_str ^ "\nexpression " ^ e2_str ^ "\nhas type " ^ t2_str)

let rec find loc x = function 
  | [] -> complain (x ^ " is not defined at " ^ (string_of_loc loc)) 
  | (y, v) :: rest -> if x = y then v else find loc x rest


(* may want to make this more interesting someday ... *) 
let rec match_types (t1, t2) = (t1 = t2) 

let make_uop loc uop (e, t) = 
    match uop, t with 
    | NEG, TEint  -> (UnaryOp(loc, uop, e), t) 
    | NEG, t'     -> report_expecting e "integer" t
    | DEREF, TEref t' -> (UnaryOp(loc, uop, e), t')
    | DEREF, t'       -> report_expecting e "reference" t

let make_bop loc bop (e1, t1) (e2, t2) = 
    match bop, t1, t2 with 
    | ADD, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | ADD, TEint,  t      -> report_expecting e2 "integer" t
    | ADD, t,      _      -> report_expecting e1 "integer" t
    | SUB, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | SUB, TEint,  t      -> report_expecting e2 "integer" t
    | SUB, t,      _      -> report_expecting e1 "integer" t
    | MUL, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | MUL, TEint,  t      -> report_expecting e2 "integer" t
    | MUL, t,      _      -> report_expecting e1 "integer" t
    | DIV, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | DIV, TEint,  t      -> report_expecting e2 "integer" t
    | DIV, t,      _      -> report_expecting e1 "integer" t
    | GTEQ, TEint, TEint  -> (Op(loc, e1, bop, e2), TEbool)
    | GTEQ, TEint, t      -> report_expecting e2 "integer" t
    | GTEQ, t,     _      -> report_expecting e1 "integer" t
    | ASSIGN, TEint, TEref TEint -> (Op(loc, e1, bop, e2), TEunit)
    | ASSIGN, TEint, t          -> report_expecting e2 "reference to integer" t
    | ASSIGN, t,     _          -> report_expecting e1 "integer" t



let rec  infer env e = 
    match e with 
    | Integer _            -> (e, TEint)
    | UnaryOp(loc, uop, e) -> make_uop loc uop (infer env e) 
    | Op(loc, e1, bop, e2) -> make_bop loc bop (infer env e1) (infer env e2) 
    | Seq(loc, el)         -> infer_seq loc env el 
    | If_then_else(loc, e1, e2, e3) ->
        let (e1', t1) = infer env e1 in 
        let (e2', t2) = infer env e2 in 
        let (e3', t3) = infer env e3 in 
        if match_types (t1, TEbool) then 
          if match_types (t2, t3) then 
            (If_then_else(loc, e1', e2', e3'), t2)
          else report_type_mismatch (e2, t2) (e3, t3)
        else report_expecting e1 "boolean" t1
    | While(loc, e1, e2) ->
        let (e1', t1) = infer env e1 in 
        let (e2', t2) = infer env e2 in 
        if match_types (t1, TEbool) then
            if match_types (t2, TEunit) then
                (While(loc, e1', e2'), TEunit)
            else report_expecting e2 "unit" t2
        else report_expecting e1 "boolean" t1
    | Var(loc, x) -> (e, find loc x env)

and infer_seq loc env el = 
    let rec aux carry = function 
      | []        -> internal_error "empty sequence found in parsed AST" 
      | [e]       -> let (e', t) = infer env e in (Seq(loc, List.rev (e' :: carry )), t)
      | e :: rest -> let (e', _) = infer env e in aux (e' :: carry) rest 
    in aux [] el 

let env_init = [] 

let check e = 
    let (e', _) = infer env_init e 
    in e' 

