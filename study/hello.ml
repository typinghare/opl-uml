(* print_int (5 * 6);;
print_string "\n";;

print_int (777 mod 42);;
print_string "\n";;

Printf.printf "Hello world!\n";;

let my_ref = ref 5;;
let value = !my_ref;;
print_int value;; *)

let rec sum u =
  match u with
  | [] -> 0
  | x::v -> x + (sum v)
;;

let arr = [1; 4; 3; 2; 5];;
print_int (sum arr);;
print_string "\n";;

let rec length u =
  match u with
  | [] -> 0
  | _::v -> 1 + (length v)
;;

print_int (length arr);;
print_string "\n";;

let square x = x * x;;
let arr = List.map square arr;;
print_int (sum arr);;
print_string "\n";;

