{
open Parser
open Printf
let incline lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+


rule read = 
  parse 
  | "+"   { PLUS }
  | "*"   { TIMES }
  | "/"   { DIV }
  | "add"   { PLUS }
  | "mul"   { TIMES }
  | "div"   { DIV }
  | ";"   { SEQ }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "["   { LBRAK }
  | "]"   { RBRAK }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "<"   { LANGLE }
  | ">"   { RANGLE }
  | "::"   { CONS }
  | ","   { COMMA }
  | "push"  { PUSH }
  | "pop"  { POP }
  | "dup"  { DUP }
  | "swap"  { SWAP }
  | "whilene"  { WHILENE }
  | "skip"  { SKIP }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof   { EOF }
  | "#" [^ '\n']* '\n' { read lexbuf } (* eat up one-line comments *)
  | white { read lexbuf }
  | _ as c { 
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1 
          }
	
