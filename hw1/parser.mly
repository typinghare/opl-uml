%{
  open Ast
%}


%token <int> INT
%token PLUS
%token TIMES
%token DIV
%token SEQ
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRAK
%token RBRAK
%token LANGLE
%token RANGLE
%token COMMA
%token CONS
%token PUSH
%token POP
%token DUP
%token SWAP
%token WHILENE
%token SKIP
%token EOF


%left SEQ 


%type <Ast.command> com
%type <int list> stack
%type <Ast.command * (int list)> config

%start config com

%%

com :
  | PUSH INT { Push $2 }
  | POP { Pop }
  | PLUS { Add }
  | TIMES { Mul }
  | DIV { Div }
  | DUP { Dup }
  | SWAP INT { Swap $2 }
  | SKIP { Skip }
  | WHILENE LBRACE com RBRACE { Whilene $3 }
  | com SEQ com { Seq ($1, $3) }
  ;

stack :
  | LBRAK RBRAK { [] }
  | INT CONS stack { $1 :: $3 }
  
config :
  LANGLE com COMMA stack RANGLE { ($2, $4) }