(* REFERENCE: http://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html and 2017 problem set*)

open Ast
open Printf
open Unix
open Eval
             
let () = 
  let _ = 
    if Array.length Sys.argv <> 2 then
      (printf "Usage: %s <inputfile> \n" Sys.argv.(0);
       exit 0) in 
  let name = Sys.argv.(1) in 
  let file = open_in name in
  let lexbuf = Lexing.from_channel file in
  let (cmd, stack) = try Parser.config Lexer.read lexbuf
                     with Parsing.Parse_error ->
                       let pos = lexbuf.Lexing.lex_curr_p in
                       printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
                       exit 1 in  
  let s = eval cmd stack (ref 5000000) in 
  List.iter (printf "%d :: ") s;
  print_string "[]\n"
  


