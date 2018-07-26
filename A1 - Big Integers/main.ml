(*Define the basic data types*)

type nat = O | S of nat;;
type sign = Pos | Neg;;
type digitseq = int list ;;
type bigint = sign * int* digitseq;;

(*Question 1*)

   (*Evaluate the maxint giving an input greater than maxint*)
   (*If input is less than maxint then output is x*)
let rec evaluate x = if (x>0) then x else evaluate (x-1);;
   (*Evaluating the maxint giving any input less than maxint*)
   (*Concept:Find a number more than maxint n then apply evaluate on it*)
let rec maxint x= if (2*x)>0 then maxint (2*x) else evaluate (2*x);;
   (*Fixing an input for maxint x*)
   (*Converting the input from int type to unit type*) 
let maxint () = maxint 2;;

(*Test input-
  # maxint ();;
- : int = 4611686018427387903*)


(*Question 2*)
     
    (*Reversing a digitseq*)
let rec reverse a = match a with
     [] -> []
   | x::xs -> (reverse xs)@x::[];;
    (*Source: COL 100 slide*)
     (*Finding the least significant digit of digitseq*)
let head  a = match a with []-> 0 | x::xs -> x;;
    (*Source: COL 100 slide*) 
    (*Compare each element of intlist with the base*) 
let rec compare (a,b) = match a with []->true | p::q-> if p<b then compare (q,b) else false;;
    (*Ensure if each element is positive or not*)
let rec nonnegative a= match a with [] -> true | x::[]-> if x>=0 then true else false | x::xs-> if x>=0 then nonnegative xs else false;;
    (*Fixing the type of input'a'in bigint as sign*) 
let asign a= match a with 
           Pos -> true | Neg -> true;;

    (*Main program*)
    (*Input-bigint ; output-bool*)
let check_bigint ((a,b,c):bigint)  = if b<= 0 then raise (Failure "base invalid") else 
 if asign a = true then  
      if nonnegative c = true then 
               if (head (reverse c))!= 0 then
                              match c with []->true
                            | x::xs -> if x<b then compare (xs,b) else raise (Failure "The elements in Digitseq have crossed base") 
               else if c = [] then true else raise (Failure "most significant digit is Zero")  
      else raise (Failure "Some of the elements are negative in digitseq")
 else raise (Failure "a doesn't has type sign");;

(*Test input-
   # check_bigint ((Pos,4,[3;2;3]):bigint);;
- : bool = true
# check_bigint ((Pos,-3,[2;1;2]):bigint);;
Exception: Failure "base invalid".
# check_bigint ((Pos,4,[4;3;4]):bigint);;
Exception: Failure "The elements in Digitseq have crossed base".
# check_bigint ((Pos,5,[-3;2]):bigint);;
Exception: Failure "Some of the elements are negative in digitseq".*)

(*Question 3*)

   (*Converting an integer into digitseq*)
let rec div (x,r) = if x=0 then [] else (x mod r:: div (x/r ,r));;
   (*Main program*)
   (*Input-integer ; Output-Bigint*)
let int2bigint (x,r) = if r>0 then
     if x>=0 then ((Pos,r,div(x,r)):bigint) 
     else ((Neg ,r,div ((-x),r)):bigint)
 else raise (Failure "Invalid base");; 
 
(*Test input-
  # int2bigint (5,3);;
- : bigint = (Pos, 3, [2; 1])
# int2bigint (-5,3);;
- : bigint = (Neg, 3, [2; 1])*)

  
(*Question 4*)
    (*Converting a digitseq into an integer*)
let rec conversion (x,r) = match x with 
     [] -> 0
  | l::[] -> l 
  | l::ls -> l+(r*(conversion(ls,r)));;
    (*Main Program*)
    (*Input-bigint ; Output-integer*)
let bigint2int ((a,b,c):bigint) = if check_bigint ((a,b,c):bigint)=true then
              match a with Pos -> conversion (c,b)
                         | Neg -> - (conversion (c,b))
             else raise (Failure "Input is wrong");;   

(*Test input -
   # bigint2int ((Pos,4,[3;2]):bigint);;
- : int = 11
# bigint2int ((Neg,4,[3;2]):bigint);;
- : int = -11 *)

(*Question 5*)
(*(a)*)

let rec nat2int a = match a with 
         O -> 0
    | (S m) -> 1+(nat2int m);;
    (*Main program*)
    (*Input-nat ; Output-bigint*)
let nat2bigint (a,r)= if r>0 then int2bigint ((nat2int a),r)
        else raise (Failure "Invalid base");;

(*Test input-
  # nat2bigint (S(S(S(O))),4);;
- : bigint = (Pos, 4, [3])*)  
  
(* Test input-
   # nat2bigint (S(S(S(O))),2);;
- : bigint = (Pos, 2, [1; 1])
# nat2bigint (O,1);;
- : bigint = (Pos, 1, [])*)

  
(*(b)*)

let rec int2nat a = match a with 
        0 -> O
      | n -> S (int2nat(n-1));;
    (*Main program*)
    (*Input-bigint ; output-Nat*)
let bigint2nat ((a,b,c):bigint) = if check_bigint ((a,b,c):bigint) = true then
            match a with Neg -> raise (Failure "nat is not neg")
                      |  Pos -> int2nat (bigint2int ((a,b,c):bigint)) 
       else  raise (Failure "Input is wrong");; 
(*Test input-
  # bigint2nat ((Pos,4,[3;2]):bigint);;
- : nat = S (S (S (S (S (S (S (S (S (S (S O))))))))))
# bigint2nat ((Neg,3,[2;1]):bigint);;
Exception: Failure "nat is not neg".*)


 
(*Question 6*)

    (*Adding three integers giving the 3 inputs n the base of bigint*)
let add' ((a,b,c),r) =  match div ((a+b+c),r) with 
          [] -> (0,0)
        | [p] -> (0,p)
        | [p;q] -> (q,p)
        | (_) ->  raise (Failure "no need") ;;
    (*Adding an integer to the least significant digit of digitseq *)
    (*inputs are digitseq,integer and a base*) 
let rec add ((ys,c),r) = match ys with 
          [] -> [c]
        | [x] -> div ((x+c),r)
        | x::xs -> match div ((x+c),r) with 
                       [] -> 0::xs | [p] -> p::xs | [p;q] -> p::add((xs,q),r)
	| (_) -> raise (Failure "no need") ;;
    (*Adding 2 digitseqs and adding an integer to the prefix of sum of lists*)
    (*Inputs are two digitseqs , integer and base*)
let rec addc ((xs,ys,c),r) = match xs,ys with 
                [],ys -> add ((ys,c),r)
              | xs,[] -> add ((xs,c),r)
              | a::a',b::b' -> let (d,e) = add' ((a,b,c),r) 
                                   in e::(addc ((a',b',d),r));;
    (*Subtracting a digitseq from another digitseq*)
let rec subtract (a,b) = match a,b with
         [],[] -> []
       | a,[] -> a
       | x::xs,y::ys -> if x>=y then (x-y)::subtract (xs,ys)
                       else (match (xs,ys) with (p::ps,[]) ->(10+(x-y))::(p-1)::ps
                                             | (p::ps,q::qs) -> (10+(x-y))::subtract((p-1)::ps,q::qs)
                                             | (_,_) -> raise (Failure"no need"))
       | (_,_) -> raise (Failure "no need") ;;
     (*Subtract a digitseq from another to get a bigint*)
let rec subtraction ((a,b),r) = if (a,b) = ([],[]) then ((Pos,r,[]):bigint) else
                     if conversion (a,r) >= conversion (b,r) then ((Pos,r,subtract(a,b)):bigint) else ((Neg ,r,subtract (b,a)):bigint);;
                                                  
     (*Main program*)
     (*Input-two bigints ; Output-bigint - Sum of the two inputs*) 
let bigplus (((a,b,c):bigint),((d,e,f):bigint)) = if check_bigint ((a,b,c):bigint) = true then 
                  if check_bigint ((d,e,f):bigint) = true then
                       if b=e then match (a,d) with 
                             (Pos,Pos) -> ((Pos,b,addc ((c,f,0),b)):bigint)
                           | (Neg,Neg) -> ((Neg,b,addc ((c,f,0),b)):bigint)
                           | (Pos,Neg) -> subtraction ((c,f),b)
                           | (Neg,Pos) -> subtraction ((f,c),b)
                       else raise (Failure "base not same")
                   else raise (Failure "Second input is wrong")
                 else if check_bigint ((d,e,f):bigint)=true then raise (Failure "First input is wrong")
                      else raise (Failure "Both inputs are wrong");;

 
(* Test input -
  # bigplus (((Pos,3,[2;1]):bigint),((Pos,3,[1;1]):bigint));;
- : bigint = (Pos, 3, [0; 0; 1])
# bigplus (((Pos,4,[3;2]):bigint),((Neg,4,[2;2]):bigint));;
- : bigint = (Pos, 4, [1; 0])
# bigplus (((Pos,3,[2;1]):bigint),((Pos,4,[2;2]):bigint));;
Exception: Failure "base not same".*)

(*Question 7*)

     (*Multiply a digitseq with an integer and add a carrybit*)
let rec mult1 ((l1,a,b),r) = match l1 with 
              [] -> [b]
            | [x] -> div(((x*a)+b),r)
            | x::xs -> let (d,e) = add' ((0,b,(x*a)),r)
                       in e::mult1 ((xs,a,d),r);;
     (*Multiply two integers and output gives the carrybit and badd*)
let mult2 ((a,b),r)= match div((a*b),r) with 
            [] -> (0,0)
          | [p] -> (0,p)
          | [p;q] -> (q,p)
          | (_) -> raise (Failure "no need");;
     (*Multiply a digitseq with a constant giving an extra input base*)
let rec mult3 ((l1,a),r) = match l1 with 
             [] -> []
           | [x] -> div ((x*a),r)
           | x::xs -> match div ((x*a),r) with 
                        [] -> 0::mult3 ((xs,a),r)
                      | [p] -> p:: mult3 ((xs,a),r)
                      | [p;q] -> p::mult1((xs,a,q),r)
                      | (_) -> raise (Failure "no need") ;;
     (*Multiply two digitseqs*)
let rec multiply ((l1,l2),r)= match l1,l2 with 
              [],[] -> []
            | [],l2 -> []
            | l1,[] -> [] 
            | l1,x::xs -> addc
              (((mult3((l1,x),r)),(0::multiply((l1,xs),r)),0),r);;
    (*Main program*)
    (*Input-Two bigints ; Output-Bigint -Product of the two inputs*) 
let rec bigtimes (((a,b,c):bigint),((d,e,f):bigint)) = if check_bigint ((a,b,c):bigint) = true then
           if check_bigint ((d,e,f):bigint)= true then
              if b=e then match (a,d) with 
                  (Pos,Pos)-> ((Pos,b,multiply((c,f),b)):bigint)
		| (Neg,Neg)-> ((Pos,b,multiply((c,f),b)):bigint)
	        | (Neg,Pos)-> ((Neg,b,multiply((c,f),b)):bigint) 
	        | (Pos,Neg)-> ((Neg,b,multiply((c,f),b)):bigint)
              else raise (Failure "Base not same")
           else raise (Failure "Second input is wrong")
       else if check_bigint ((d,e,f):bigint) = true then raise (Failure "First input is wrong")
            else raise (Failure "Both inputs are wrong");;

  (* Test case-
      # bigtimes (((Pos,4,[3;2]):bigint),((Neg,4,[2;2]):bigint));;
- : bigint = (Neg, 4, [2; 3; 2; 1])
# bigtimes (((Pos,3,[2;1]):bigint),((Pos,3,[1;1]):bigint));;
- : bigint = (Pos, 3, [2; 0; 2])*)


