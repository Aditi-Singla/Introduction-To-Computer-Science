module Matrices = struct;;
exception ZeroDiv;;
type complex = float * float;;
let eps = 1E-12;;
let re ((a,b): complex) = a;;
let im ((a,b): complex) = b;;

(* magcx finds the magnitude of a complex number *)
let magcx ((a,b):complex) = sqrt(a*.a +. b*.b);;

(* conjugcx finds the complex conjugate of a complex number *)
let conjugcx ((a,b): complex) : complex = (a, -.b);;

(* real2complex takes a float and returns a complex number with input float as its real part *)
let real2complex a : complex = (a, 0.);;

(* addinvcx finds the additive inverse of a complex number i.e. the number that when added to a complex number returns 0 *)
let addinvcx ((a,b): complex) : complex = (-.a,-.b);;

(* addcx & addc adds two complex numbers *)
let addcx c1 c2 : complex =((re c1)+.(re c2), (im c1)+.(im c2));;
let addc (c1,c2) : complex =((re c1)+.(re c2), (im c1)+.(im c2));; 

(* multcx & multc multiplies two complex numbers *)
let multcx (c1,c2) : complex = let a1 = re c1 and b1 = im c1
                             and a2 = re c2 and b2 = im c2
                             in
                             (a1*.a2 -. b1*.b2, a1*.b2 +. a2*.b1);;
let multc c1 c2 : complex = let a1 = re c1 and b1 = im c1
                             and a2 = re c2 and b2 = im c2
                             in
                             (a1*.a2 -. b1*.b2, a1*.b2 +. a2*.b1);;

(* recipcx takes a complex number c and returns a complex number e such that multcx c e returns 1 *)
let recipcx c : complex = let a = re c and b = im c
                          in
                          let d = (magcx c)*.(magcx c)
                          in if d <= eps then raise ZeroDiv
                             else (a/.d, -. b/.d);;
exception Dimension;;
exception Unequal;;
exception Negative of int;;
type vectorcx = complex list;;
type matrixcx = complex list list;;
let head a = match a with
    [] -> raise (Failure "Empty list")
  | x::xs -> x;;
let tail x = match x with
    [] -> raise (Failure "empty list")
  | _::ys -> ys;;
let rec length a =  match a with
      [] -> 0
    | x::xs -> 1 + (length xs);;
let rec map f l = match l with
                  [ ] -> [ ]
                | (x::xs) -> (f x)::(map f xs);;
let rec zip l1 l2 = match (l1, l2) with
                       ( [ ] , [ ] ) -> [ ]
                     | ( x::xs, [ ] ) -> raise Unequal
                     | ( [ ], y::ys ) -> raise Unequal
                     | (x::xs, y::ys) -> (x,y) :: (zip xs ys );;

(*To check for a valid matrix*)
(*Null matrix is [] only, no other representation is accepted*)
let rec isMatrix (a:matrixcx) = match a with 
     [] -> true
   | [x] -> true
   | x::xs -> if length x = 0 then false else
                if length x = length (head xs) then isMatrix xs
              else false;; 

(*Question 1*)

(*To drop first n elements of the list*)
let rec drop n l = if n<0 then raise (Negative n)
                   else match l with
                           [ ] -> [ ]
                         | x::xs -> if n = 0 then l
                                    else (drop (n-1) xs);;

(*To remove the null matrices from front of a given list*)
(*It will be used to get [] frm [[];[];[]] & so on*)
let rec correct a= match a with [] -> [] | x::xs -> if x=[] then correct xs
                                                        else a;;

(*Main program*)
(*To find the transpose of a given matrix*)
let rec transpose (a:matrixcx) :matrixcx = if isMatrix a then 
                              match a with [] -> []
                                      | x::xs -> let rec headlist b = match b with  [] -> []
                                                                      |	x::xs -> head x ::headlist xs
                                                 in  (headlist a)::transpose (correct (map (drop 1) a))
              else raise Dimension;;    


(*Question 2*)

exception Multiplication_not_possible;;

(*To find dimensions of a vector*)
let dimvcx (a:vectorcx) = length a;;

(*To find dimensions of a matrix*)
let dims (a: matrixcx) = if isMatrix a then
                              match a with 
                                [] -> (0,0)
                             | x::xs -> (length a,dimvcx x)
                          else raise Dimension;;

(*To add all the complex numbers in a list*)
let rec add1 (a:vectorcx) = match a with [] -> (0.,0.)
			   |  x::xs -> addcx x (add1 xs);;

(*To multiply the corresponding terms of two vectors & add them all*)    
let rec mult1 (a:vectorcx) (b:vectorcx) = add1 (map multcx (zip a
								b));;
(*Main Program*)
(*To multiply two matrices*)
let rec matmult (a:matrixcx) (b:matrixcx) : matrixcx = if isMatrix a & isMatrix b then
         match (a,transpose b) with 
               ([],b) -> []
	           | (a,[]) -> []
             | (x::xs,y::ys) -> let ((m,n),(p,q)) = (dims a,dims b)
    in 
    if n=p then (map (mult1 x) (transpose b)):: matmult xs b
    else raise Multiplication_not_possible
   else raise Dimension ;;

(*Question 3*)

exception No_such_row_exists ;;

(*Main program*)
(*To delete ith row of a matrix*)
let rec delrow i (a:matrixcx) :matrixcx = if isMatrix a then             
              match a with
                 [] -> raise No_such_row_exists
            | x::xs -> if i<0 then raise (Negative i) else 
                       if i=0 then a else
                       if i=1 then xs else x::(delrow (i-1) xs)
else raise Dimension;; 

(*Question 4*) 
 
exception No_such_column_exists ;;

(*Main program*)
(*To delete ith column of a matrix*)
let delcol i (a:matrixcx) :matrixcx = let rec delcol1 i a = if isMatrix a then             
              match a with
                 [] -> raise No_such_column_exists
            | x::xs -> if i<0 then raise (Negative i) else 
                       if i=0 then a else
                       if i=1 then xs else x::(delcol1 (i-1) xs)
           else raise Dimension
           in 
           transpose (delcol1 i (transpose a));; 

(*Question 5*)

(*To multiply a scalar to a complex*)
let scalmult a b = let a1 = re b and b1 = im b
                   in 
                   (a*.a1 , a*.b1);;

(*To find exponent a^b where a is a float & b is an int & output is float*)
let rec power a b = if b<0 then raise (Failure "invalid")
                    else if b=0 then 1. else a*.(power a (b-1));;

(*To delete a row without checking the validity of a matrix*) 
let rec delrow1 i (a:matrixcx) :matrixcx =  match a with
                 [] -> raise No_such_row_exists
            | x::xs -> if i<0 then raise (Negative i) else 
                       if i=0 then a else
                       if i=1 then xs else x::(delrow1 (i-1) xs);;

(*Main program*)
(*To find the determinant of a given square matrix*)
let rec determinant (a:matrixcx) = if isMatrix a
                    then let (m,n) = dims a
                         in
                         if m=n then let rec tdet a i s=  match a with 
                                           [] -> s
                     		                | [[x]] -> x
                                        | x::xs ->  if x= [] then s else 
                      tdet ((tail x)::xs) (i+1) (addcx (scalmult (power (-1.) (i+1)) (multc (head x) (determinant (delcol i (delrow1 1 a)))))  s) 
                in tdet a 1 (0.,0.)
  else raise (Failure "Not a square matrix")
else raise Dimension  ;; 

(*Test cases*)
(*
# transpose [[(2.,4.);(4.5,6.)];[(2.,6.5)]];;
  Exception: Dimension.
# transpose [[(2.,4.);(4.5,6.)];[(2.,6.5);(3.,4.)]];;
- : matrixcx = [[(2., 4.); (2., 6.5)]; [(4.5, 6.); (3., 4.)]]
# transpose [[(2.,4.);(4.5,6.);(2.,3.)];[(2.,6.5);(3.,4.);(2.,4.)];[(2.,1.);(3.,7.6);(5.,8.5)]];;
- : matrixcx =
  [[(2., 4.); (2., 6.5); (2., 1.)]; [(4.5, 6.); (3., 4.); (3., 7.6)];
  [(2., 3.); (2., 4.); (5., 8.5)]]
# matmult [[(2.,3.);(3.,6.)];[(5.,3.);(4.,5.)]] [[(3.,4.)];[(5.,4.)]];;
- : matrixcx = [[(-15., 59.)]; [(3., 70.)]]
# matmult [] [[(2.,3.)];[(2.,4.)]];;
- : matrixcx = []
# matmult [] [[(2.,3.)];[(2.,4.);(2.,6.7)]];;
  Exception: Dimension.
# delrow 2 [];;
  Exception: No_such_row_exists.
# delrow 3 [[(2.,4.);(4.5,6.)];[(2.,6.5)]];;
  Exception: Dimension.
# delrow 3 [[(2.,4.);(4.5,6.)];[(2.,6.5);(3.,4.)]];;
  Exception: No_such_row_exists.
# delrow 3 [[(2.,4.);(4.5,6.);(2.,3.)];[(2.,6.5);(3.,4.);(2.,4.)];[(2.,1.);(3.,7.6);(5.,8.5)]];; 
- : matrixcx =
  [[(2., 4.); (4.5, 6.); (2., 3.)]; [(2., 6.5); (3., 4.); (2., 4.)]] 
# delcol 1 [];;
  Exception: No_such_column_exists.
# delcol 3 [[(2.,4.);(4.5,6.)];[(2.,6.5)]];;
  Exception: Dimension.
# delcol 3 [[(2.,4.);(4.5,6.)];[(2.,6.5);(3.,4.)]];;
  Exception: No_such_column_exists.
# delcol 3 [[(2.,4.);(4.5,6.);(2.,3.)];[(2.,6.5);(3.,4.);(2.,4.)];[(2.,1.);(3.,7.6);(5.,8.5)]];; 
- : matrixcx =
[[(2., 4.); (4.5, 6.)]; [(2., 6.5); (3., 4.)]; [(2., 1.); (3., 7.6)]]
# determinant [];;
- : complex = (0., 0.)
# determinant [[(2.,3.)]];;
- : complex = (2., 3.)
# determinant [[(1.,1.);(2.,2.)];[(4.,4.);(5.,5.)]];;
- : complex = (0., -6.)
# determinant [[(1.,1.);(2.,2.);(3.,3.)];[(4.,4.);(5.,5.);(6.,6.)];[(7.,7.);(8.,8.);(9.,9.)]];;
- : complex = (0., 0.)
#  determinant [[(1.,1.);(2.,2.)];[(4.,4.);(5.,5.)];[(7.,7.);(8.,8.)]];;
   Exception: Failure "Not a square matrix".
 *)
 
end;;
