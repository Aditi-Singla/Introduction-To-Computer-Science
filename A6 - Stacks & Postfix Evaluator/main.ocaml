(*Define a module STACK*)
module STACK = struct ;;

(*Define a type 'a stack*)
type 'a stack = 'a list;;
(*Define type opcode listing all the operators & Num(x)*)
type opcode = Num1 of float | Plus | Times | Divide | Subtract | Power
	      | Sin | Cos | Tan | Exp | Log | Squareroot | Modulus ;; 
(*Define a type exptree*)
type exptree =  Num of float | P of opcode * exptree * exptree;;
(*Define epsilon which tends to zero*)
let eps = 1e-12;;
(*Raising various exceptions*)
exception Invalid_division;;
exception Log_of_neg;;
exception Sqrt_of_neg;; 

(*Evaluating various operations*)
(*Inputs are opcode operator & a tupple of two floats*)
(*Func : val eval : opcode -> float * float -> float*)
let rec eval t (a,b) = match t with
               Num1(x)    -> x
             | Plus      -> a +. b
             | Times     -> a *. b
             | Divide    -> if abs_float(b) < eps then raise Invalid_division
                                  else b/.a
             | Subtract  -> b -. a
             | Power     -> b**a 
             | Sin       -> sin (a+.b)
             | Cos       -> cos (a+.b)
             | Tan       -> tan (a+.b)
             | Exp       -> exp (a+.b)
             | Log       -> if (a+.b) <= eps then raise Log_of_neg else log (a+.b)
             | Squareroot-> if (a+.b) <= eps then raise Sqrt_of_neg else sqrt (a+.b)
	    	 | Modulus   -> abs_float (a+.b);;

(*To pop the top two elements of a stack*)
(*Func : val pop : float stack -> float * float*)
let pop (l:float stack) = match l with
              [] -> raise (Failure "Not possible")
            | [x] -> raise (Failure "Not possible")
            | x::y::_ -> (x,y);;

(*To get a stack with top two elements popped out*)
(*Func : val top : float stack -> float list*)
let top (l:float stack) = match l with 
               [] -> raise (Failure "Not possible")
            | [x] -> raise (Failure "Not possible")
            | x::y::z -> z;;

(*To get the postorder traversal of an exptree as an opcode list*)
(*Func :  val postordv : exptree -> opcode list *)
(*Space : O(n) where n is the size of the exptree*)
(*Time : O(2^n) where n is the height of the exptree*)
let rec postordv t = match t with
             Num (x)  -> [Num1 (x)]
          |  P(x,y,z) -> (postordv y)@(postordv z)@[x];;

(*To evaluate the final answer from the postorder traversal*)
(*It will insert the elements of the postorder into a stack one by one *)
(*if it is num else it will pop 2 elements & apply the operator on *)
(*them & will then insert the answer into the stack*)
(*Inputs: Opcode list & an empty stack*)
(*Func : val evaluate : opcode list * float stack -> float stack*)
(*Time :O(n) where n is the size of the exptree*)
(*Space: O(2^n) where n is the height of the exptree*)
let rec evaluate (t,(s:float stack)) = match (t) with
                []      -> s
            | Num1 (x)::y -> evaluate (y,x::s)
            | x::y -> evaluate (y,(eval x (pop s))::(top s));;  

(*It just takes the exptree as an input & apply evaluate on its *)
(*postorder traversal*)
(*Func : val solve : exptree -> float stack*)
(*Space: O(2^n) where n is the height of the exptree*)
(*Time: O(n) where n is the size of the exptree*)
let solve t = evaluate (postordv t,[]);;

(*Test cases*)
let t1 =
  (P(Plus,Num(2.),P(Times,P(Sin,Num(1.),Num(0.)),P(Divide,P(Exp,Num(-7.1),Num(0.)),Num(3.6)))));;

let t2 =
  (P(Tan,Num(0.),P(Modulus,Num(0.),P(Plus,P(Times,Num(31.6),P(Cos,Num(3.14),Num(0.))),P(Squareroot,Num(0.),P(Times,Num(2.9),P(Squareroot,Num(0.0),P(Divide,Num(2.1),Num(3.)))))))));;

let t3 =
  P(Plus,Num(3.),P(Plus,Num(-2.3),P(Times,Num(9.1),P(Log,Num(0.),P(Plus,Num(13.),P(Times,P(Divide,Num(12.9),P(Modulus,Num(0.),P(Subtract,Num(2.3),Num(4.5)))),P(Squareroot,Num(0.),Num(6.25))))))));;

let t4 =
  P(Squareroot,Num(0.),P(Plus,Num(2.),P(Squareroot,Num(0.),P(Plus,Num(2.),P(Squareroot,Num(0.),P(Times,Num(2.),P(Squareroot,Num(0.),P(Plus,Num(2.),P(Squareroot,Num(0.),P(Plus,Num(2.),Num(2.)))))))))));;

let t5 =
  P(Plus,Num(34.1),P(Times,P(Subtract,Num(0.),P(Log,Num(0.),P(Modulus,Num(0.),P(Subtract,P(Times,Num(3.4),Num(6.8)),P(Exp,Num(0.),Num(11.5)))))),P(Sin,Num(0.0),P(Times,Num(1.07),P(Squareroot,Num(0.),Num(4.))))));;

end;;

open STACK;;
solve t1;;
solve t2;;
solve t3;;
solve t4;;
solve t5;;
