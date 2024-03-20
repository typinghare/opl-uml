open Ast

exception InvalidStack
exception InvalidOperands
exception Timeout

(* replace function for swap *)
(* https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list?rq=1 *)
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l

(* command evaluation *)
let rec eval (cmd : command) (stack : int list) (fuel: int ref): (int list) =
  if !fuel = 0 then
    raise Timeout
  else
    fuel := !fuel - 1;
  match cmd with
  | Push n        -> n :: stack
  | Pop           -> (match stack with 
                        h :: tl -> tl
                      | _ -> raise InvalidStack
                     )
  | Add           -> (match stack with 
                        h1 :: h2 :: tl -> (h1 + h2) :: tl
                      | _ -> raise InvalidStack
                     ) 
  | Mul           -> (match stack with 
                        h1 :: h2 :: tl -> (h1 * h2) :: tl
                      | _ -> raise InvalidStack
                     )
  | Div           -> (match stack with 
                        n :: m :: tl -> (m / n) :: tl
                      | _ -> raise InvalidStack
                     )
  | Dup           -> (match stack with 
                        h1 :: tl -> h1 :: h1 :: tl
                      | _ -> raise InvalidStack
                     )
  | Swap n        -> if n > List.length stack then raise InvalidStack
                     else
                       let nth = List.nth stack n in
                       let head = (List.hd stack) in
                       let stack' = replace stack n head in
                       replace stack' 0 nth
  | Seq (c1, c2)  -> let stack' = eval c1 stack fuel in eval c2 stack' fuel
  | Whilene c     ->  (match stack with 
                         h1 :: h2 :: _ -> if h1 = h2 then stack
                                          else
                                            let stack' = eval c stack fuel in
                                            eval cmd stack' fuel 
                       | _ -> raise InvalidStack
                      )
  | Skip -> stack
          
