open Ast

exception IllformedExpression 

let rec eval_arith ev op e1 e2 : value = 
  (match eval ev e1 with
  | VInt n1 -> 
    (match eval ev e2 with
      | VInt n2 -> 
        VInt (op n1 n2)
      | _ -> 
        raise IllformedExpression)
  | _ -> 
    raise IllformedExpression)

(* large-step evaluation function
   Complete remaining cases of e: exp
   Refer to ast.ml for exp and value definitions.
 *)
and eval (ev: env) (e: exp) : value = 
  match e with 
  | Var x ->
    (try VarMap.find x ev
      with Not_found -> raise IllformedExpression) 
  | Int n -> VInt n
  | True -> VBool true
  | False -> VBool false
  | Lam(x, e) -> VFun(ref ev, x, e)
  | Let(x, e1, e2) -> 
    eval (VarMap.add x (eval ev e1) ev) e2
  | App(e1, e2) ->
    let e1' = eval ev e1 in
    let e2' = eval ev e2 in
    begin
      match e1' with
      | VFun(vref, vvar, vexp) ->
        eval (VarMap.add x e2' !vref) vexp
      | _ -> raise IllformedExpression
    end
  | Plus(e1, e2) -> 
    let e1' = eval ev e1;
    let e2' = eval ev e2;
    eval_arith e1' ( + ) e2'
  | Times(e1, e2) ->
    let e1' = eval ev e1;
    let e2' = eval ev e2;
    eval_arith e1' ( * ) e2'
  | Minus(e1, e2) ->
    let e1' = eval ev e1;
    let e2' = eval ev e2;
    eval_arith e1' ( - ) e2'
  | Pair(e1, e2) ->
    VPair(eval ev e1, eval ev e2)
  | Fst(e) ->
    begin
      match eval ev e' with
      | VPair(v1, v2) -> v1
      | _ -> raise IllformedExpression
    end
  | Snd(e) ->
    begin
      match (eval ev e') with
      | VPair(v1, v2) -> v2
      | _ -> raise IllformedExpression
    end
  | Eq(e1, e2) ->
    (match (eval ev e1) in
    | VInt v1 -> 
      (match (eval ev e2) in 
      | VInt v2 -> if v1 = v2 then VBool(true) else VBool(false)
      | _ -> raise IllformedExpression
      )
    | _ -> raise IllformedExpression)
  | If(e1, e2, e3) ->
    match (eval ev e1) with
    | VBool(r) -> if r then (eval ev e2) else (eval ev e3)
    | _ -> raise IllformedExpression
  | _ -> raise IllformedExpression
