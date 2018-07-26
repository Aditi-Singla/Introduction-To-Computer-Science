module Polynomials = struct;;
type polynomial = float list;;
let head (l:polynomial) = match l with [] -> 0. | x::_ -> x;;
let tail (l:polynomial) = match l with [] -> raise (Failure "Empty list") | _::ys -> (ys:polynomial);;
let rec reverse (l:polynomial) = match l with [] -> [] | x::xs-> (((reverse xs) @ (x::[])):polynomial);;

(*Correction : To eliminate zeroes at the end of the list*)

let rec correction (l:polynomial) = if l = [] then [] else
                  if head (reverse l) = 0. then ((correction (reverse (tail (reverse l)))):polynomial)
                  else (l:polynomial);;
let rec length (l:polynomial) = match (correction l) with [] -> 0. | x::xs -> 1.+.(length xs);;

(*To insert a given no. of zeroes in front of the list*)
(*Inputs - List & a no. (no. of zeroes)*)

let rec normalisation (l:polynomial) a =if a<0. then raise (Failure "Negative a") else
                                     if a=0. then l 
                                     else normalisation (0.::l) (a-.1.);;
let rec map f (l:polynomial) = match l with 
          [] -> []
       |  x::xs -> (((f x)::(map f xs)):polynomial);;
let multfc x y = x*.y ;;

(*Question 1*)
(*To evaluate the value of a polynomial for a given input using tail rec*)

let evaluate (l:polynomial) a = let rec tevaluation (l:polynomial) a s = match correction (reverse l) with 
                                                  [] -> 0.
                                                | x::xs -> if xs = [] then s else 
                                                           tevaluation (reverse xs) a (s*.a +. head xs)
                                in
                                tevaluation l a (head (reverse l));;

(*Question 2*)
(*To add two polynomials*)

let rec addpoly (a:polynomial) (b:polynomial) = match (correction a,correction b) with
     (a,[]) -> a
   | ([],b) -> b
   | (x::xs,y::ys) -> (x+.y)::addpoly xs ys;;

(*Question 3*)
(*To multiply two polynomials using tail rec*)
 
let multpoly a b = let rec tmult (a:polynomial) (b:polynomial) i s = match b with 
                                                   [] -> correction s
                                                 | x::xs -> tmult a xs (i+.1.) (addpoly (normalisation (map (multfc x) a) i) s)
                   in
                   tmult (correction a) (correction b) 0. [];;

(*Question 4*)
(*To find the derivative of a polynomial using tail rec*)

let derivpoly l = let rec tderivpoly (l:polynomial) i (s:polynomial) = match l with 
                                                               [] -> ((correction (reverse s)):polynomial)
                                                             | x::xs -> tderivpoly xs (i+.1.) (((head xs)*.i)::s)
                  in
                  tderivpoly (correction l) 1. [];;

(*Question 5*)
(*To divide a polynomial by another polynomial using tail rec*)

let neg a = -.a;;
let rec subtractpoly (a:polynomial) (b:polynomial) = match (correction a,correction b) with
     (a,[]) -> a
   | ([],b) -> map neg b
   | (x::xs,y::ys) -> (correction ((x-.y)::subtractpoly xs ys));;
let rec remainder a b = if (correction b = [])  then raise (Failure "Not defined")
		else if length (correction a) < length (correction b) then (correction a) else
                     let x = map (multfc ((head (reverse a))/.(head (reverse b)))) (normalisation b (length a-.length b))
		     in 
                     let y = subtractpoly a x
                     in 
                     remainder y b;; 

let quotient a b = let rec tquotient a b s = if (correction b = [])  then raise (Failure "Not defined")
		                    else if length (correction a) < length (correction b) then (correction s) else
                                    let x = map (multfc ((head (reverse a))/.(head (reverse b)))) (normalisation b (length a-.length b))
		                    in 
                                    let y = correction (subtractpoly a x)
                                    in                          
                                    tquotient y b (addpoly (normalisation [(head(reverse a))/.(head (reverse b))] (length a-.length b)) s)
                    in   
                    tquotient (correction a) (correction b) [];;                     
let dividepoly a b = ((quotient a b),(remainder a b));;
end;;
