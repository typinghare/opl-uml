/* Examples for testing */

(ref (lambda x. x));
(! (ref (lambda x. x)));


x/;
y/;
z/;
      
(lambda x. x) ( (ref (lambda z. z)));
(lambda x. x) (! (ref (lambda u. u)));

 (lambda y. (ref z) := ! y) (ref (lambda a. a))  ;

 (lambda y. (ref z) := ! (! y)) (ref (lambda a. a))  ;

! (ref (lambda a. a))  ;
ref x := ref (lambda x. x x);
ref (lambda k. k) := ref (lambda x. x x);

! (ref (lambda x. x)) ;    
