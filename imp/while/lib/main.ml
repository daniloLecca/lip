open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* BIG-STEP SEMANTICS (for expressions) *)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const n -> Nat n
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Nat(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) when n1 >= n2 -> Nat(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Nat(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          



(* SMALL-STEP SEMAMTICS (for commands) *)

(* bottom state *)
let bot = fun x -> raise (UnboundVar x)

(* keeps the same bindings and adds the new binding st[x/v] *)
let bind st x v : state = fun y -> if y=x then v else st y

let rec trace1 = function
    St _ -> raise NoRuleApplies (* no more commands to be executed *)
  | Cmd(c,st) -> match c with (* if the command is a Skip, go to the next command *)
      Skip -> St st
    | Assign(x,e) -> let v = eval_expr st e in St (bind st x v) (* assign a value to a variable *)
    | Seq(c1,c2) -> (match trace1 (Cmd(c1,st)) with (* Seq stands for ;*) (* execute the command before the ; *)
          St st1 -> Cmd(c2,st1) (* the first command has been executed so now the second command can be executed *)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1)) (* the first command has been executed and after that there is another command, 
                                                   so execute the command after that and finally execute the command c2*)
    | If(e,c1,c2) -> (match eval_expr st e with (* if true execute the first command else the second command *)
          Bool true -> Cmd(c1,st)
        | Bool false -> Cmd(c2,st)
        | _ -> raise (TypeError "If"))
    | While(e,c) ->  (match eval_expr st e with (* while true do c;while condition do command *)
          Bool true -> Cmd(Seq(c,While(e,c)),st)
        | Bool false -> St st
        | _ -> raise (TypeError "While"))


let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(* a program in the language can be pictured as Cmd(Cmd(.., st^n), bottom)*)

(* takes a command and returns the trace of the commands *)
let trace n t = trace_rec n (Cmd(t,bot))