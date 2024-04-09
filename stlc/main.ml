open Ast

let () =
  let usage_exit () = (Format.printf "Usage: lam <file>\n"; exit 0) in
  let filearg = 1 in
  let file = open_in (Sys.argv.(filearg)) in
  let lexbuf = Lexing.from_channel file in
  let e =
    try Parser.exp Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1 in

  
  let _ = 
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    Pprint.printExp e;
    Format.printf "@]@\n@\n" in

  (* Utility function for grading to rename the type variables in result to a canonical form *)
  let canon_name t =
    let m = ref VarMap.empty in
    let c = ref 2600 in
    let rec visit s =
      match s with
      | TInt | TBool -> s
      | TVar y ->
         (if not (VarMap.mem y !m) then
            let i = !c in
            incr c;
            m := VarMap.add y (Helper.tvar_name i) !m
         );
         TVar (VarMap.find y !m)
      | TPair(t1,t2) ->
         let t1' = visit t1 in
         let t2' = visit t2 in
         TPair(t1', t2')
      | TArrow(t1,t2) ->
         let t1' = visit t1 in
         let t2' = visit t2 in
         TArrow(t1', t2')
    in
    visit t
  in
  
  let _ = 
    Format.printf "Evaluating the expression...@\n";
    Format.print_flush () in
  
  let v = Eval.eval VarMap.empty e in

  let _ = 
    Format.printf "Result:@\n  @[";
    Pprint.printVal v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()
