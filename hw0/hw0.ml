(*
It's my first time writing OCaml code, and I think it's a pretty special language. It's easy to
create a fibonacci function in C/C++ or Java. So allow me to translate Java to OCaml here.
    // Use recursive technique to simplify the function. The parameter n is an integer ranging from
    // 0 to infinity. Note that fibonacci(0) = 0 fibonacci(1) = 1;
    int fibonacci(int n) {
        return n <= 1 ? n : fibonacci(n - 1) + fibonacci(n - 2);
    }

The following OCaml code create a fibonacci function that takes an `n` as parameter. The keyword
`rec` indicates that this functions can call itself.
*)
(* let rec fibonacci n =
    if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2);;

(* Test case: calculate and print the 10th Fibonacci number *)
let fibonacci_10th_num = fibonacci(10);;
Printf.printf "The 10th Fibonacci number is: %d" fibonacci_10th_num;;

let rec sum = function
 | [] -> 0
 | head::others -> head + sum others

let a = [1 ; 2; 3];;
let b = 0 :: a;;  (* b = [0; 1; 2; 3] *)
let s: int = sum b;;
sum s; *)

type exp =
 | EVar of string
 | EInt of int
;;

let rec eval e env =
    match e with
    | EVar x -> env x
    | EInt n -> n
;;

