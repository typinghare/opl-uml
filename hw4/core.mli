(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

type store
val emptystore : store
val shiftstore : int -> store -> store 
val eval : context -> store ->term -> term * store 
