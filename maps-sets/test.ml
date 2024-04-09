type var = string

(* maps with variables as keys *)
module VarMap = Map.Make(struct
  type t = var
  let compare = Stdlib.compare
end)

(* maps with variables as keys *)
module VarSet = Set.Make(struct
  type t = var
  let compare = Stdlib.compare
end)

exception MapError

(* Create an empty map *)
let test1 = VarMap.empty

(* Create a singleton map with x mapped to 2 *)
let test2 = VarMap.add "x" 2 VarMap.empty

(* Expand map returned in test2 with y mapped to 3 *)
let test3 = VarMap.add "y" 3 test2

(* lookup x from the map returned in test3;
   return 0 if not found
 *)
let test4 = 
   try VarMap.find "x" test3 
   with Not_found -> 0

(* lookup z from the map returned in test3;
   return 0 if not found
 *)
let test4b = 
   try VarMap.find "z" test3 
   with Not_found -> 0

exception SetError

(* Create an empty set *)
let test5 = VarSet.empty

(* Create a singleton set with the element x *)
let test6 = VarSet.singleton "x"

(* Extend the set returned in test6 with the element y *)
let test7 = VarSet.add "y" test6


(* lookup x from the set returned in test7;
   return x if found;
   return string "Not found" if there is no such element *)
let test8 = 
   try VarSet.find "x" test7
   with Not_found -> "Not found"

(* lookup z from the set returned in test7;
   return z if found;
   return string "Not found" if there is no such element *)
let test9 = 
   try VarSet.find "z" test7
   with Not_found -> "Not found"

(* lookup x from the set returned in test7;
   return true if found;
   return false if there is no such element *)
let test10 = VarSet.mem "x" test7

(* lookup z from the set returned in test7;
   return true if found;
   return false if there is no such element *)
let test11 = VarSet.mem "z" test7

(*  Return a set that is union of test7 and { "z" } *)
let test12 = VarSet.union test7 (VarSet.singleton "z")


