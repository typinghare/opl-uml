open Ast
open Printf
open Unix
open String
open Eval
          

let run_test cmd input expected =
  try
    let output = (eval cmd input (ref 5000000)) in
    
    if expected = output then
      printf "  Yes\n"
    else
      (printf "  Failed: expected ";
       List.iter (printf "%d :: ") expected;
       printf "[], but got ";
       List.iter (printf "%d :: ") output;
       printf "[]\n"
      )
  with Timeout -> printf "  Failed: timeout\n"
     | InvalidStack -> printf "  Failed: invalid stack\n"
  
let rec mainloop () =
  printf "\nDo you want to run code for (1) Question 4c, (2) Question 4d, or (3) Question 4e? ";
  let c = read_int() in
  if c < 1 || c > 3 then mainloop () else (); 
  printf "Paste program: (empty line to end)\n";
  let rec accumulate_input a =
    let s = read_line () in
    if length s = 0 then a else accumulate_input (a^" "^s)
  in
  let prog = accumulate_input "" in
  let lexbuf = Lexing.from_string prog in
  let _ =
    try
      let cmd = Parser.com Lexer.read lexbuf in
      if c = 1 then
        (* run some test cases for 4a *)
        (run_test cmd [0] [0];
         run_test cmd [1] [0;1];
         run_test cmd [2] [0;1;2];
         run_test cmd [3] [0;1;2;3];
         run_test cmd [7] [0;1;2;3;4;5;6;7])
      else if c = 2 then
        (* put in some test cases here for 4b *)
        ( run_test cmd [2] [2;1;0];
          run_test cmd [0] [0];
          run_test cmd [1] [1;0];
          run_test cmd [3] [3; 2; 1; 0];
          run_test cmd [7] [7; 6; 5; 4; 3; 2; 1; 0])
      else 
        (* put in some test cases here for 4c*)
        ( 
          run_test cmd [14; 7] [0];
          run_test cmd [7; 14] [1];
(*           run_test cmd [0; 1] [0];
 *)          run_test cmd [12; 36; 7; 9; 10; 8] [1; 7; 9; 10; 8];
          run_test cmd [36; 12; 7; 9; 10; 8] [0; 7; 9; 10; 8];
        )
      
    with Parsing.Parse_error ->
      printf "Syntax error"
  in
  mainloop ()
  
let () =  mainloop ()
        
         


