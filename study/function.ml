(*
In OCaml, a function is considered as an expression. But it's not actually an expression, because
the expression is not evaluated until the function is called. Since the RHS of a function assignment
statement is an expression, there is no scope. Therefore, when we want to create a local variable in
a function, we add an `in` keyword at the end of an assignment, like this:

    let product = a * b in

In which the `let` and `in` wrap an assignment statement and they separate the statement from the
return value.
*)
let calculator (input1: int) (input2: int): int =
    let product = input1 * input2 in
    product;;

(* Call the calculator function *)
print_int (calculator 3 5);;
print_string "\n";;

(*
The body of the following function is like:

    return switch (x) {
        case 0:
            return "true";
        case 1:
            return "false";
        default:
            return "undefined";
    }

Note that the underscore (_) is a wildcard pattern, and it matches everything.
*)
let is_zero (x: int) : string =
    match x with
        | 0 -> "true"
        | 1 -> "false"
        | _ -> "undefined";;

(* Call the is_zero function *)
print_string (is_zero 0);;
print_string "\n";;
print_string (is_zero 1);;
print_string "\n";;
print_string (is_zero 2);;
print_string "\n";;