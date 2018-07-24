module Stack = struct ;;
type 'a stack = 'a list;;
type adj_list = (int*(int list)) list ;;
exception Empty_list;;
let create x = ([]:'a stack);;
let push x (l:'a stack) = ((x::l):'a stack);;
let isEmpty (l:'a stack) = if l = [] then true else false;;
let size (l:'a stack) = (List.length l);;
let top (l:'a stack) = if l = [] then raise Empty_list
                          else List.hd l;;
let pop (l:'a stack) = if l = [] then raise Empty_list
                          else ((List.tl l):'a stack);;

let rec foldl f e l = match l with
                [ ] -> e
          | (x::xs) -> f x (foldl f e xs);;
(*To map a function on a list*)

(*f : ('a -> 'b)*)
let rec map f l = match l with 
                     [] -> []
                | x::xs -> f x ::(map f xs);;
(*f : ('a -> 'b list) *)
let rec map1 f l = match l with 
                     [] -> []
                | x::xs -> (f x)@(map1 f xs);;

(*To check if a is a member of a stack b*)
let rec ismember a  l = match l with 
                       [] -> false
                  | x::xs -> if x = a then true else ismember a xs;;
(*To remove the last element of a list*)
let remove l = match (List.rev l) with 
         [] -> []
       |x::xs -> List.rev xs;;
(*To check if head & tail of a list are same or not*)
let rec tail l = match l with
                       [] -> true
                  | x::xs -> if (List.hd (List.rev l)) = x then true
                       else false;;
(*To apply the tail function on a list of lists & return a bool*)
let checktail l = let conj x y = x or y
                  in
                  foldl conj false (map tail l);;
(*To return list of lists which give true for tail function*)
let rec check1 l= match l with 
        [] -> []
      | x::xs -> if (tail x) then x::(check1 xs) else (check1 xs);;
(*To return list of lists which donot give true for tail function*)
let rec check2 l = match l with 
        [] -> []
      | x::xs -> if (tail x) then (check2 xs) else x::(check2 xs);;
(*To check if in each list of a list of lists, the last element lies *)
(*anywhere in the list*)  
let rec checkmember l= match l with 
        [] -> false
      | x::xs ->  if (ismember (List.hd (List.rev x)) (remove x)) then true else checkmember xs;;
(*To return the list of lists which give false for checkmember*)
let rec check3 l = match l with 
        [] -> [] 
      | x::xs ->  if (ismember (List.hd (List.rev x)) (remove x)) then check3 xs
		  else x::(check3 xs);;
(*To check if two lists have same elements or not*)
let rec seteq a b= let rec subseteq a b = match a with 
                                    [] -> true
                               | x::xs -> if ismember x b then subseteq xs b else false
                   in
                   if subseteq a b then subseteq b a  else false;;
(*To avoid repetition of loops*)
let rec repetition l = let rec isseteq a b= match b with
          [] -> false
	| x::xs -> if seteq a x then true else isseteq a xs
        in
       match l with 
        [] -> [] 
   | x::xs -> if (isseteq x xs ) then repetition xs else
		x::(repetition xs);;

(*For checking a valid graph*)

(*To make a list of all nodes*)
let rec allnodes g = match g with
               [] -> []
      | (a,l)::xs -> a::(allnodes xs);;
(*To make a list of all the neighbours of all the nodes*)
let rec allneighbours g = match g with
               [] -> []
      | (a,l)::xs -> l@(allneighbours xs);;    
(*To check for a valid graph*)
(*It equates the two lists made above*)
let validgraph (g:adj_list) = seteq (allnodes g) (allneighbours g);;    

(*To find all the neighbours of a node*)
let rec neighbours (g:adj_list) a = match g with
                       [] -> raise (Failure "Graph is invalid")
              | (x,l)::xs -> if x = a then l else neighbours xs a;;


(*It pairs all the neighbours with its nodes one by one*)
let rec pair g = let pairup a b = [a;b]
                 in
                 match g with 
                        [] -> []
               | (a,l)::xs -> (map (pairup a) l)@pair xs;;
(*It further adds the neighbours of tail of above function one by one*)
let add g a = let rec add1 g a l  = match a with
                    [] ->  []
               | x::xs -> if l = [] then []
                        else (a @ [(List.hd l)])::(add1 g a (List.tl l))
              in
              add1 g a (neighbours g (List.hd (List.rev a)));;

(*It formulates all the loops possible in a graph*)
let rec loops g l s = match l with 
        [] -> repetition s
     | x::xs -> if (checktail l) then loops g (check2 l) ((check1 l) @ s)
                else if (checkmember l) then loops g (check3 l) s
                     else loops g (map1 (add g) l) s;;

(*It first checks the validity of an adjacency list & then applies loops on it*)
(*Space : O(n*(2^n))*)
(*Time : O(n*(2^n))*)
let cycles (g:adj_list) = if (validgraph g) then (loops g (pair g) []) else raise (Failure "Invalid graph");;
let long_cycle (g:adj_list) = let rec long l s = match l with 
                                         [] -> s
                                       | x::xs -> if (List.length x > List.length s) then (long xs x)
						  else long xs s
                                 in
                                 long (cycles g) [];;
(*Time : O(n) where n is the number of cycles*)
(*Space :O(n) where n is the number of cycles*)
let longest_cycles (g:adj_list) = let n = List.length (long_cycle g) 
                                 in 
                                 let rec list l s = match l with 
                                    [] -> s
                                  | x::xs -> if (List.length x) = n then (list xs (x::s)) else (list xs s)
                                 in
                                 list (cycles g) [];;

end;;

open Stack;;
cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1])];;
cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[])];;
cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1]);(6,[6])];;
cycles [(1,[1;2]);(2,[3]);(3,[5;4]);(4,[5]);(5,[3;1])];;
cycles [(1,[2;3]);(2,[3]);(3,[1]);(4,[5;6]);(5,[6]);(6,[4])];;
longest_cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1])];;
longest_cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[])];;
longest_cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1]);(6,[6])];;
longest_cycles [(1,[1;2]);(2,[3]);(3,[5;4]);(4,[5]);(5,[3;1])];;
longest_cycles [(1,[2;3]);(2,[3]);(3,[1]);(4,[5;6]);(5,[6]);(6,[4])];;
cycles [(1,[2;3]);(2,[3]);(3,[])];;
cycles [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1;6])];;
longest_cycle [(1,[2;3]);(2,[3]);(3,[])];;
longest_cycle [(1,[3;4]);(2,[1]);(3,[2;4]);(4,[5]);(5,[1;6])];;



