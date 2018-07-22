type 'a set = 'a list;;

(*Question 1*)
(*Define an emptyset*)

let emptyset : 'a set = [];;

(*Question 2*)
(*Check if given element is a member of given set*)

let rec member (a:'a) (b:'b set) = match (b:'b set) with 
             [] -> false 
           | x::xs -> if x=a then true else member (a:'a) (xs:'b set);;

(*Question 3*)
(*Check if first set is a subset of another set*)

let rec subseteq (a:'a set) (b:'b set) = match a with 
    [] -> true
  | x::xs -> if member x b then subseteq xs b 
             else false;;

(*Question 4*)
(*Check the equality of two given sets*)

let rec seteq (a:'a set) (b:'b set)= if subseteq a b then subseteq b a 
                                     else false;;

(*Question 5*)
(*To find set of elements belonging to a & not b*)
(*To ensure the non repetition define a function inside*)

let rec setdiff (a:'a set) (b:'b set) = match a with 
      [] -> []
    |  x::xs -> let rec repetition (a:'a set) = match a with 
                  [] -> []
               |  x::xs -> if member x xs then ((x::repetition (setdiff xs [x])):'a set)
               else ((x::repetition xs):'a set)
    in
    if member x b then repetition (setdiff xs b) 
              else ((repetition (x::(setdiff xs b))):'a set);;

(*Define a function to avoid repetition*)

let rec repetition (a:'a set) = match a with 
      [] -> []
    | x::xs -> if member x xs then ((x::repetition (setdiff xs [x])):'a set)
               else ((x::repetition xs):'a set);; 

(*Question 6*)
(*To find the union of given two sets*)

let rec union (a:'a set) (b:'b set) = match b with 
      [] -> repetition a
    | x::xs -> if member x a then repetition (union a xs)
               else repetition (union (x::a) xs);;

(*Question 7*)
(*To find the intersection of given two sets*)

let rec intersect (a:'a set) (b:'b set) = match a with 
      [] -> []
    | x::xs -> if member x b then repetition (x::intersect xs b) 
               else repetition (intersect xs b);;

(*Question 8*)
(*To find the powerset*)

(*To insert an element in front of a list*)
(*As in COL 100 slides*)

let insertf a (l:'a set) = ((a::l):'a set);;

(*To insert an element in every set of a given superset*)
let rec map a (b:'a set set) = match b with
    [x] -> [insertf a x]
  | x::xs-> (((insertf a x) :: map a xs):'a set set)
  |  []-> raise (Failure " Invalid entry") ;;

(*Main program*)
let rec powerset a= match repetition a with  
    []-> [[]]
  | [x]-> [[];[x]]
  | x::xs -> let pa=powerset xs
             in 
             (((map x pa)@pa):'a set set);;

(*Question 9*)

(*To map the first element of set a with every element of set b*)
let rec cartesian1 (a:'a set) (b:'b set) = match a,b with 
     [],b -> []
   | a,[] -> []
   | x::xs,y::ys -> ((repetition((x,y)::cartesian1 a ys)):'c set);;
 
(*Main Program*)
(*To map every element of set a with every element of b using previous function*)

let rec cartesian (a:'a set) (b:'b set) = match a,b with   
     [],b -> []
   | a,[] -> []
   | x::xs,y::ys -> ((repetition ((cartesian1 a b)@(cartesian xs b))):'c set);; 
