(*Rep invar : list*)
(*Define a type stack*)
type 'a stack = 'a list ;;

(*Define a type move to show the movement of a ring from one peg to another*)
type move = FromTo of int*int;;

(*Define a signature module type having a type t*)
module type TYPE =
sig
 type t
end ;;

(*Define a module type Stack with a functor Set of type TYPE*)
module type STACK =
functor (Abc:TYPE) ->
sig
 type elt = Abc.t (*elt stands for an element*)
 exception Empty_list
 val create: unit -> elt stack
 val push: elt -> elt stack -> elt stack
 val isEmpty: elt stack -> bool
 val size: elt stack -> int
 val top: elt stack -> elt
 val pop: elt stack -> elt stack
end ;;

(*Now define a module Stack of defined signature & use the previous functor*)
module Stack:STACK =
functor (Abc:TYPE) ->
struct
 open Abc
 type elt = t
 exception Empty_list
 let create x = ([]:elt stack)
 let push (x:elt) (l:elt stack) = ((x::l):elt stack)
 let isEmpty (l:elt stack) = if l = [] then true else false
 let size (l:elt stack) = (List.length l)
 let top (l:elt stack) = if l = [] then raise Empty_list
                          else ((List.hd l):elt)
 let pop (l:elt stack) = if l = [] then raise Empty_list
                          else ((List.tl l):elt stack)
end ;;

(*Define modules of different types*)
module Int = struct
type t = int
end ;;
module Float = struct
type t = float
end ;;
module String =  struct
type t = string
end ;;

(*PART B*)

module Int = Stack (Int) ;;
open Int ;;

exception Negative of int;;
exception Invalid_position;;
exception Not_possible;;

(*DESCR: We will put the first element on the last*)
(*n by recursion append the reverse of the tail to the first element*)
(*FUNC : reverse : 'a list -> 'a list*)
(*TIME: O(n)*)
(*SPACE: O(n)*)   
let rec reverse l = match l with 
                    []    -> [] 
                  | y::ys -> (reverse ys)@(y::[]);;

(*DESCR : To check if a given element is a member of a given list*)
(*FUNC :  member : 'a -> 'a list -> bool*)
(*TIME : O(n) *)
(*SPACE : O(n)*)
let rec member a b = match b with 
             [] -> false 
           | x::xs -> if x=a then true else member a xs;;

(*DESCR : To check if stacks are sorted & have no element in common*)
(*FUNC : checkinput:'a Stack.stack*'a Stack.stack*'a*Stack.stack -> bool*)
(*TIME : O(n)*)
(*SPACE : O(n)*)
let checkinput (s1,s2,s3) = 
             let rec checksort l = (match l with
                                     [] -> true
                                  | [x] -> true
                                | x::xs -> if (x < (top xs)) then                          checksort xs
                                    else false)
                          in
                          let rec compare (s1,s2) = match s2 with 
                                               [] -> true  
   			           | x::xs -> if (member x s1) then false
                                          else compare (s1,xs)
    in  
    
(checksort s1)&(checksort s2)&(checksort s3)&(compare (s1,s2)) &(compare (s2,s3))&(compare (s3,s1));;              

(*DESCR : It gives various moves for different values of i&j*)
(*FUNC : move :  int * int * 'a Stack.stack * 'a Stack.stack * 'a Stack.stack ->
  Stack.move * 'a Stack.stack * 'a Stack.stack * 'a Stack.stack *)
(*TIME :O(1)*)
(*SPACE : O(1)*)
(*REP INVAR : i,j = peg number & s1,s2,s3 = config of each peg*) 
let move (i,j,s1,s2,s3) = (match (i,j) with
                                               (1,2)-> (FromTo (1,2), (pop s1) , (push (top s1) s2) , s3)
                                             | (1,3)-> (FromTo (1,3), (pop s1) , s2 , (push (top s1)  s3))
                                             | (2,1)-> (FromTo (2,1), (push (top s2) s1) , (pop s2) , s3)
                                             | (2,3)-> (FromTo (2,3), s1 , (pop s2) , (push (top s2) s3))
                                             | (3,1)-> (FromTo (3,1), (push (top s3) s1) , s2 , (pop s3))
                                             | (3,2)-> (FromTo (3,2), s1 , (push (top s3) s2) , (pop s3))
                                             | (_,_)-> raise (Failure "No such peg exists"));;  

(*DESCR : It first moves n-1 rings from i to k ,then nth from i to j & *)
(*then n-1 rings from k to j*)
(*FUNC :  hanoi1 : int * int * int * int * int Stack.stack * int Stack.stack * int Stack.stack ->
  (Stack.move * int Stack.stack * int Stack.stack * int Stack.stack) list*)
(*TIME : O(2^n)*)
(*SPACE : O(n)*)
(*REP INVAR : m is the no. of rings to be moved from i to j using k *)
(*& s1,s2,s3 are the initial configuration of three pegs*)

let rec hanoi1 (m,i,j,k,(s1:int stack),(s2:int stack),(s3:int stack))=
                                (if (m < 0) then raise Not_possible
                                else if (m = 0) then []
                                else(if (m = 1) then [move (i,j,s1,s2,s3)]
                                     else (let (FromTo (_,_) , a, b, c)= (List.hd (reverse (hanoi1 (m-1,i,k,j,s1,s2,s3))))
                                           in
                                           let (FromTo (_,_) , d, e, f)= move (i,j,a,b,c)
                                           in
                                           ((hanoi1
					       (m-1,i,k,j,s1,s2,s3)) @ ((move(i,j,a,b,c))::(hanoi1 (m-1,k,j,i,d,e,f)))))));;

(*DESCR : It just uses hanoi function after checking the input pegs*)
(*FUNC :  hanoi : int * int * int * int * int Stack.stack * int Stack.stack * int Stack.stack ->
  (Stack.move * int Stack.stack * int Stack.stack * int Stack.stack) list*)
(*TIME : O(2^n)*)
(*SPACE : O(n)*)
(*REP INVAR : m is the no. of rings to be moved from i to j using k *)
(*& s1,s2,s3 are the initial configuration of three pegs*)
let hanoi (m,i,j,k,(s1:int stack),(s2:int stack),(s3:int stack)) = if checkinput (s1,s2,s3) then
                          hanoi1 (m,i,j,k,(s1:int stack),(s2:int stack),(s3:int stack))      
                           else raise (Failure "The pegs are not sorted or have an element in common");;

(*TEST CASES:
# hanoi (2,1,2,3,[1;2;3],[],[]);;
- : (move * Int.elt stack * Int.elt stack * Int.elt stack) list =
[(FromTo (1, 3), [2; 3], [], [1]); (FromTo (1, 2), [3], [2], [1]);
 (FromTo (3, 2), [3], [1; 2], [])]
# hanoi (3,2,1,3,[1;2;3],[],[]);;  
Exception: Stack(Abc).Empty_list.
# hanoi (4,1,2,3,[1;2;3],[],[]);;
Exception: Stack(Abc).Empty_list.
# hanoi (2,1,2,3,[1;2;2],[],[]);;
Exception: Failure "The pegs are not sorted or have an element in common".
# hanoi (2,4,2,3,[1;2;3],[],[]);;
Exception: Failure "No such peg exists".
*)


                                                                                                                 
