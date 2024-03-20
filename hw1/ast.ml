(*************************************************************
   Type for command syntax
 ************************************************************)

type command = 
  | Push of int
  | Pop
  | Add
  | Mul
  | Div
  | Dup
  | Swap of int
  | Seq of command * command
  | Whilene of command
  | Skip
