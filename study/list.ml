(* Function to print a list of integers *)
let rec print_int_list (_list: int list) =
    match _list with
        | [] -> ()
        | [x] -> print_int x
        | x::remainder ->
            print_int x;
            print_string " ";
            print_int_list remainder
;;


let list0: int list = [2; 3; 4];;
print_int_list list0;;
print_string "\n";;

(* unshift 1 to list 0 *)
let list1 = 1::list0;;
print_int_list list1;;
print_string "\n";;

(* Print the length of a list *)
print_int (List.length list1);;
print_string "\n";;

(* Find the summation of a list *)
let rec sum (_list: int list) =
    match _list with
        | [] -> 0
        | x::remainder -> x + sum(remainder)
;;

print_int (sum list1);;
print_string "\n";;
