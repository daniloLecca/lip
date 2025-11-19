open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

  
let rec trace1_expr st = function
  | False -> (False, st)
  | True -> (True, st)
  | Var x -> (Const (apply st x)), st
  | Const n -> (Const n, st)
  | Not(True) -> (False, st)
  | Not(False) -> (True, st)
  | Not(e) -> let (e', st') = trace1_expr st e in (Not(e'), st')
  | And(True,e) -> (e, st)
  | And(False,_) -> (False, st)
  | And(e1,e2) -> let (e1', st') = trace1_expr st e1 in (And(e1',e2), st')
  | Or(True,_) -> (True, st)
  | Or(False,e) -> (e, st)
  | Or(e1,e2) -> let (e1', st') = trace1_expr st e1 in (Or(e1',e2), st')  
  | Add (Const n1, Const n2) -> (Const (n1 + n2), st)
  | Add(Const n,e) -> let (e', st') = trace1_expr st e in (Add(Const n,e'), st')
  | Add(e1,e2) -> let (e1', st') = trace1_expr st e1 in (Add(e1',e2), st')
  | Mul (Const n1, Const n2) -> (Const (n1 * n2), st)
  | Mul(Const n,e) -> let (e', st') = trace1_expr st e in (Mul(Const n,e'), st')
  | Mul(e1,e2) -> let (e1', st') = trace1_expr st e1 in (Mul(e1',e2), st')
  | Sub (Const n1, Const n2) -> (Const (n1 - n2), st)
  | Sub(Const n,e) -> let (e', st') = trace1_expr st e in (Sub(Const n,e'), st')
  | Sub(e1,e2) -> let (e1', st') = trace1_expr st e1 in (Sub(e1',e2), st')
  | Eq (Const n, Const n') -> ((if n = n' then True else False), st)
  | Eq (Const n, e) -> let (e', st') = trace1_expr st e in (Eq(Const n, e'), st')
  | Eq (e1, e2) -> let (e1', st') = trace1_expr st e1 in (Eq(e1', e2), st')
  | Leq (Const n, Const n') -> ((if n <= n' then True else False), st)
  | Leq (Const n, e) -> let (e', st') = trace1_expr st e in (Leq(Const n, e'), st')
  | Leq (e1, e2) -> let (e1', st') = trace1_expr st e1 in (Leq(e1', e2), st')
  | Call(f,Const(n)) -> (match (topenv st) f with (* if the function has been called*)
        IFun(x,c,er) -> (* and if f is a function *)
        let l = getloc st in (* get the first free location *)
        let env' = bind (topenv st) x (IVar l) in (* get the new environment with the binding x |-> l*)
        let mem' = bind (getmem st) l n in (* get the memory and make the assignment l <- n *)
        let st' = make_state (env'::(getenv st)) mem' (l+1) in (* create a new environment with the allocated parameter *)
        (CallExec(c,er), st') (* execute the body of the function *)
      | _ -> raise (TypeError "Call of a non-function"))
  | Call(f,e) -> let (e',st') = trace1_expr st e in (Call(f,e'),st') (* make a step to evaluate the expression *)
  | CallExec(c,e) -> (match trace1_cmd (Cmd(c,st)) with (* execute all the commands in the body and finally return the return expression*)
      St st' -> (CallRet(e),st')
    | Cmd(c',st') -> (CallExec(c',e),st')) (* the return expression is already known at runtime (for example like "return x + 10") *)
  | CallRet(Const(n)) -> (* remove the environment of the current function and return the return value*)
      let st' = make_state (popenv st) (getmem st) (getloc st) in 
      (Const(n),st')
  | CallRet(e) -> let (e',st') = trace1_expr st e in (CallRet(e'),st')

and trace1_cmd = function (* we need to declare the two function with let ... and ... because they call eachother and they're not declared at the same time*)
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,Const(n)) -> (* when an assignment is made, the name 'x' should belong to a variable not to a function *)
        (match (topenv st) x with
         | IVar l ->
             let mem' = bind_mem (getmem st) l n in
             St (setmem st mem')
         | IFun _ -> raise (TypeError (Printf.sprintf "Can't assign to function %s" x)))
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st') 
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')
    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),st)



(* performs one single declaration *)
let sem_decl1 (e,l) = function
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Fun(f,x,c,er) -> let e' = bind e f (IFun(x,c,er)) in (e',l)

(* performs a list of declarations *)
  let sem_decl (e,l) decls = 
  List.fold_left sem_decl1 (e,l) decls

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

let trace n (Prog(d,c)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c, make_state ([e]) (botmem) (l)))