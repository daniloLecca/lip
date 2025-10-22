open Ast

type exprval = Bool of bool | Nat of int;;

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



(* BIG-STEP SEMANTICS *)

let rec eval = function 
| True -> Bool true
| False -> Bool false
| Not (e) -> (match eval e with Bool b -> Bool (not b) | _ -> failwith "error")
| And(e1, e2) -> (match (eval e1, eval e2) with (Bool b1, Bool b2) -> Bool (b1 && b2) | _ -> failwith "error")
| Or(e1, e2) -> (match (eval e1, eval e2) with (Bool b1, Bool b2) -> Bool (b1 || b2) | _ -> failwith "error")
| If(e0, e1, e2) -> (match eval e0 with Bool b -> if b then eval e1 else eval e2 | _ -> failwith "error")
| Zero -> Nat 0
| Succ n -> (match eval n with Nat x -> Nat (x + 1) | _ -> failwith "error")
| Pred n -> (match eval n with Nat x when x > 0 -> Nat (x - 1) | _ -> failwith "error")
| IsZero(e) -> (match eval e with | Nat n -> Bool (n=0) | _ -> failwith "error")



(* SMALL-STEP SEMANTICS *)

exception NoRuleApplies

let rec is_nv = function 
| Zero -> true
| Succ (n) -> is_nv n
| _ -> false;;
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Pred(Zero) -> raise NoRuleApplies
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Succ(n)) when is_nv n -> n
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(n)) when is_nv n -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies;;


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;