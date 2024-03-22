(* print_int (5 * 6);;
print_string "\n";;

print_int (777 mod 42);;
print_string "\n";;

Printf.printf "Hello world!\n";;

let my_ref = ref 5;;
let value = !my_ref;;
print_int value;; *)

let rec print_int_list lst =
  match lst with
  | [] -> ()  (* If the list is empty, do nothing *)
  | x :: xs ->  (* If the list is not empty *)
      print_int x;  (* Print the current element *)
      print_string " ";  (* Print a space after the element *)
      print_int_list xs  (* Recursively print the rest of the list *)
;;


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
print_string "\n";;

let rec filter predicate list =
  match list with
  | [] -> []
  | x::v -> 
    let v' = filter predicate v in
    if predicate x then x::v' else v'
;;

let arr = filter (fun a -> a > 1) arr;;
print_int_list arr;;
