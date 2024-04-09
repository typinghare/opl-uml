type var = string

(* maps with variables as keys *)
module VarMap : (Map.S with type key = var)

(* maps with variables as keys *)
module VarSet : (Set.S with type elt = var)

val test1: int VarMap.t

(* Create a singvalon map with x mapped to 2 *)
val test2 :int VarMap.t

(* Expand map returned in test2 with y mapped to 3 *)
val test3 :int VarMap.t 

(* lookup x from the map returned in test3 *)
val test4  : int

(* lookup z from the map returned in test3 *)
val test4b  : int

exception SetError

(* Create an empty set *)
val test5 : VarSet.t 

(* Create a singvalon set with the element x *)
val test6  : VarSet.t

(* Extend the set returned in test6 with the element y *)
val test7  : VarSet.t

(* lookup x from the set returned in test7 *)
val test8  : string

(* lookup z from the set returned in test7 *)
val test9  : string

(* lookup x from the set returned in test7;
   return true if found;
   return false if there is no such element *)
val test10 :  bool

(* lookup z from the set returned in test7;
   return true if found;
   return false if there is no such element *)
val test11 : bool

(*  Return a set that is a union of test7 and { "z" }
 *)
val test12 : VarSet.t
