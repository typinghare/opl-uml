/* Examples for testing */

true;                               /* true */
if false then true else false;      /* false */
0;                                  /* 0 */
pred 0;                             /* 0 */
succ (pred 0);                      /* 1 */
pred (succ (succ 0));               /* 1 */
iszero 0;                           /* true */
iszero (pred (succ (succ 0)));      /* false */
