(*
 *  Comp 3010 Organization of Programming Languages
 *
 *  Homework 2 Question 3
 *
 *
 *
 *  Instructions
 *
 *  Please fill in the placeholder functions per the written assignment
 *  Please do not change the signature of the functions
 *  Make sure that this file loads without any errors in a
 *     clean OCaml shell
 *
 *)



(*************************************************************

   Type for lambda terms

 ************************************************************)


(* Lambda term *)
type lterm = 
    LId of string           (* Variable *)
  | LLam of string * lterm  (* Abstraction *)
  | LApp of lterm * lterm   (* Applicatoin *)



(*************************************************************

   Module implementing some helper functions
   
   LambdaParser.parse : string -> lterm
   LambdaParser.pp : lterm -> string

 ************************************************************)

module LambdaParser = struct

  (* Create a lexer to parse the code. Lexer can break the code, which is a string, into
     tokens. *)
  let lexer = Genlex.make_lexer ["("; ")"; "."; "/"]

  let lex s = 
    let str = lexer (Stream.of_string s) in
    let rec loop () = 
      match (Stream.peek str) with
      | None -> []
      | Some _ -> let elt = Stream.next str in elt::(loop()) in
    loop ()

  let expect elt cs = 
    match cs with
    | f::cs when f = elt -> Some cs
    | _ -> None

  let expect_ident cs = 
    match cs with
    | (Genlex.Ident id)::cs -> Some (id, cs)
    | _ -> None

  let rec parse_term cs = 
    match parse_ident_terms cs with
    | Some x -> Some x
    | None -> 
	(match parse_lambda cs with
	|	Some x -> Some x
	|	None ->
	    (match parse_group_terms cs with
	    | Some x -> Some x
	    | None -> 
		(match parse_ident cs with
		|	Some x -> Some x
		|	None -> 
		    (match parse_group cs with
		    | Some x -> Some x
		    | None -> None))))

  and parse_ident_term cs = 
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) -> 
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp(term1,term2),cs))

  and parse_ident_terms cs =    (* ident term+ *)
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) -> 
	(match parse_terms cs with
	|	None -> None
	|	Some (f, cs) -> Some (f term1,cs))

  and parse_group_terms cs =    (* group term+ *)
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_terms cs with
	|	None -> None
	|	Some (f,cs) -> Some (f term1, cs))

  and parse_terms cs = 
    match parse_ident cs with
    | Some (term1,cs) -> 
	(match parse_terms cs with
	|	None -> Some ((fun t -> LApp(t,term1)),cs)
	|	Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
    | None-> 
	(match parse_group cs with
	|	Some (term1,cs) -> 
	    (match parse_terms cs with
	    | None -> Some ((fun t -> LApp(t,term1)),cs)
	    | Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
	|	None -> None)
	  
  and parse_ident cs =
    match expect_ident cs with
    | None -> None
    | Some (id,cs) -> Some (LId id,cs)

  and parse_lambda cs = 
    match expect (Genlex.Kwd "/") cs with
    | None -> None
    | Some cs -> 
	(match expect_ident cs with
	|	None -> None
	|	Some (id,cs) -> 
	    (match expect (Genlex.Kwd ".") cs with
	    | None -> None
	    | Some cs -> 
		(match parse_term cs with
		|	None -> None
		|	Some (term,cs) -> Some (LLam (id,term),cs))))

  and parse_group_term cs =
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp (term1,term2),cs))

  and parse_group cs =
    match expect (Genlex.Kwd "(") cs with
    | None -> None
    | Some cs ->
	(match parse_term cs with
	|	None -> None
	|	Some (term,cs) ->
	    (match expect (Genlex.Kwd ")") cs with
	    | None -> None
	    | Some cs -> Some (term,cs)))

  let parse str = 
    match parse_term (lex str) with
    | Some (term,[]) -> term
    | _ -> failwith ("Cannot parse "^str)

  let rec pp(term: lterm) = 
    match term with
    | LId x -> x
    | LLam (x,t) -> "/"^x^"."^(pp t)
    | LApp (t1,t2) -> 
      let t1' = (match t1 with
      | LLam _ -> "("^(pp t1)^")"
      | _ -> pp t1) in
      let t2' = (match t2 with
      | LApp _ -> "("^(pp t2)^")"
      | LLam _ -> "("^(pp t2)^")"
      | _ -> pp t2) in
      t1'^" "^t2'
end
    
(* PART A *)

(**
 @brief Extracts the free variables in a lambda term. Note that we use list data structure in
        this function, which means the returned list may contain duplicate elements.
 @param term A lambda term to analyze.
 @return A list of free variables found in the lambda term.
 *)
let rec fv(term: lterm) : string list =
  match term with
  (* FV(x) = x *)
  | LId x -> [x] 
  (* FV(λx.t) = FV(t) - {x} *)
  | LLam (x, t) -> List.filter ((<>) x) (fv t)
  (* FV(t1 t2) = FV(t1) ∪ FV(t2) *)
  | LApp (t1, t2) -> (fv t1) @ (fv t2)

(**
 @brief Creates a new name for a variable.
 @param var_name The variable name.
 @return A new name the the variable. If the variable is x, this function will return x_0.   
 *)
let new_var (var_name: string) : string = 
  var_name ^ "_0"

(**
 @brief Substitutes y for z in m. (m{z/y})
*)
let rec substitute_string (m: lterm) (y: string) (z: string): lterm =
  match m with
  | LId (x) -> if x = y then LId (z) else m
  | LLam (x, body) -> if x = y then 
    LLam (z, substitute_string body y z) else
    LLam (x, substitute_string body y z)
  | LApp (t1, t2) -> LApp(substitute_string t1 y z, substitute_string t2 y z)

(**
 @brief Substitues e for x in m. (m{e/x})
 @param m The term to be operated.
 @param x The string to search for.
 @param e The term to replace the found string. 
 *)
let rec substitute (m: lterm) (x: string) (e: lterm) : lterm =
  match m with
  (* y{e/x} = x == y ? e : y *)
  | LId y -> if x = y then e else m
  (* (λy.e'){e/x} = x == y ? λy.e' : T(y, e', e, x) *)
  (* T = y not in FV(e) ? λy.(e'{e/x}) : λz.((e'{z/y}){e/x}) *)
  (* where z = new_var_name y *)
  | LLam (y, body) ->
    if x = y then m else
      let fv_of_e = fv e in   (* fv_of_e = FV(e) *)
      if not (List.mem y fv_of_e) then
        LLam(y, substitute body x e)
      else
        let new_y = new_var y in
        LLam(new_y, substitute (substitute_string body y new_y) x e)
  (* (e1 e2){e/x} = e1{e/x} e2{e/x} *)
  | LApp (e1, e2) -> LApp (substitute e1 x e, substitute e2 x e)


(* PART B *)

(**
 @brief Reduces a specified lambda expresison by applying the one-step reduction relation.
 @param A lambda expression to reduce.
 *)
let rec reduce (t: lterm): lterm option =
  match t with
  (* λx.e t => (λx.e){arg/x} *)
  | LApp (LLam(x, e), t) ->
    Some (substitute e x t)
  (* (e1 e2) => e1 can be reduced to e1' ? (e1' e2) :
    (e2 can be reduced to e2' ? (e1 e2') : None) *)
  | LApp (e1, e2) ->
    begin
      match reduce e1 with
      | Some e1' -> Some (LApp (e1', e2))
      | _ ->
        begin
          match reduce e2 with
          | Some e2' -> Some (LApp (e1, e2'))
          | _ -> None
        end
    end
  (* λx.e => e be reduced to e' ? λx.e' : None *)
  | LLam (x, e) ->
    begin
      match reduce e with
      | Some e' -> Some (LLam (x, e'))
      | _ -> None
    end
  (* If t is a LId, return None *)
  | _ -> None


(* PART C *)

(**
 @brief Returns the normal form of lambda expression t.
 @param t The lambda expression to normalize.
 *)
let rec normal_form (t : lterm) : lterm =
  match reduce t with
  | Some t' -> normal_form t'
  | _ -> t


let eval (input: string) : string =
  let term = LambdaParser.parse input in
  LambdaParser.pp (normal_form term)

(* PART D *)

(* abbreviation list type *)
type abbs = (string * lterm) list

(* sample encodings *)

let encs = let p = LambdaParser.parse in
           [ ("true", p "/x./y.x");
	     ("false", p "/x./y.y");
	     ("if", p "/c./x./y.c x y");
	     ("and", p "/b./c.b c false");
	     ("or", p "/b./c.b true c");
	     ("not", p "/b.b false true");
	     ("_0", p "/f./x.x");
	     ("_1", p "/f./x.(f x)");
	     ("_2", p "/f./x.(f (f x))");
	     ("_3", p "/f./x.(f (f (f x)))");
	     ("_4", p "/f./x.(f (f (f (f x))))");
	     ("_5", p "/f./x.(f (f (f (f (f x)))))");
	     ("succ", p "/n./f./x.f (n f x)");
	     ("plus", p "/m./n./f./x.(m f) (n f x)");
	     ("times", p "/m./n./f./x.m (n f) x");
	     ("iszero", p "/n.n (/x.false) true");
	     ("pred", p "/n./f./x.n (/g.(/h.h (g f))) (/u.x) (/u.u)");
	     ("Y", p "/f.(/x.f (x x)) (/x.f (x x))");
	     ("fact", p "Y (/fact./n.(iszero n) _1 (times n (fact (pred n))))") ]

(**
 @brief Returns the normal form of t in which abbreviations A are used; applies the transformation in encs,
  and then uses normal_form to find the normal form of the resulting term.
 @param abbs The abbreviation list.
 @param t The lambda expression to anaylize.
 *)
let normal_form_abbs (abbs: abbs) (t: lterm) : lterm =
  let rec apply_abbs (term: lterm) (abbs: abbs) : lterm =
    match term with
    | LId x ->
      begin
        (* Find "x" in abbs and return an option of the lterm *)
        match List.assoc_opt x abbs with
        (* Note that we have to recursively apply the tranformation, until there are nothing to be substitute. *)
        (* | Some t' -> apply_abbs t' abbs *)
        | Some t' -> t'
        | _ -> LId x
      end
    | LLam (x, e) -> LLam (x, apply_abbs e abbs)
    | LApp (e1, e2) -> LApp (apply_abbs e1 abbs, apply_abbs e2 abbs)    
  in
  let term_with_abbs = apply_abbs t abbs in
  normal_form term_with_abbs

(**
 @brief   
 *)
let eval_abbs (abbs: abbs) (input: string) : string =
  let term = LambdaParser.parse input in
  LambdaParser.pp (normal_form_abbs abbs term)
