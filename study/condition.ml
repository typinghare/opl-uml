(* a is the bigger number of 5 and 3 *)
let a = if (5 > 3) then 5 else 3;;
print_int a;;
print_string "\n";;

(* declare the type of b *)
let b: int = if (3 == 1) then 1 else if (3 == 2) then 2 else 3;;
print_int b;;
print_string "\n";;

let c: bool = a = b;;
let print_bool b = print_string (if b then "true" else "false");;
print_bool c;;
print_string "\n";;