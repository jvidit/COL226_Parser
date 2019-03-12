open Signature_a0


module A0 : BigInt = struct

type bigint = sign * int list
    and sign = Neg | NonNeg

exception Division_by_zero
exception Not_implemented

(*helper functions*)

let rec rem_zeros (l:int list) =					(*given a list, removes all leading zeros*)
	match l with 
	[] -> []   |
	[x] -> [x] |
	x::xs -> 
			 if x=0 then
				rem_zeros xs
			 else
			 	l
;;






let rec gt_mag (ll1:int list) (ll2:int list) = 		(*given 2 unsigned int lists, gives the truth value of l1>l2*)
	let l1=rem_zeros ll1 in
	let l2=rem_zeros ll2 in
	let len1=List.length l1 in
	let len2=List.length l2 in
	if (len1>len2) then
		true
	else if(len1<len2) then
		false
	else if(len1+len2 = 0) then
		false
	else
		let h1=List.hd l1 in
		let h2=List.hd l2 in
		if(h1>h2) then
			true
		else if(h1<h2) then
			false
		else
			gt_mag (List.tl l1) (List.tl l2)
;;

let rev_sign (s:sign) = match s with				(* reversal function for sign *)
	Neg -> NonNeg|
	NonNeg -> Neg
;;


let rec add_mag (l1:int list) (l2:int list) carry = 										(* takes 2 unsigned lists in reversed form, adds, and returns the answer in reversed form with leading zeros*)
	match (l1,l2) with
	([],[]) -> [carry] |
	_ -> 
		let a1 = match l1 with 
					[] -> 0 |
					x::xs -> x 
		in
		let a2 = match l2 with 
					[] -> 0 |
					x::xs -> x 
		in
		let lt1 = match l1 with
					[] -> [] |
					x::xs -> xs
		in
		let lt2 = match l2 with
					[] -> [] |
					x::xs -> xs
		in
		((a1+a2+carry) mod 10)::(add_mag lt1 lt2 ((a1+a2+carry)/10))
;;


let rec sub_mag (l1:int list) (l2:int list) borrow = 					(* takes 2 unsigned lists in reversed form, subtracts, and returns the answer in reversed form with leading zeros. Also assumed l1>l2*)
	match (l1,l2) with
	([],[]) -> [0] |
	_ -> 
		let a1 = match l1 with 
					[] -> 0 |
					x::xs -> (x - borrow) 
		in
		let a2 = match l2 with 
					[] -> 0 |
					x::xs -> x 
		in
		let lt1 = match l1 with
					[] -> [] |
					x::xs -> xs
		in
		let lt2 = match l2 with
					[] -> [] |
					x::xs -> xs
		in
		if (a1 < a2) then
			(a1+10-a2)::(sub_mag lt1 lt2 1)
		else
			(a1-a2)::(sub_mag lt1 lt2 0)
;;


(*helper function : multiplies an integer with an int list*)
let rec mult_single (l:int list) (n:int) (carry:int) = 				   (*takes a list in reverse with a single integer and returns the product in reverse*)
	match l with 
	[] -> [carry] |
	x::xs ->  ((n*x + carry) mod 10) :: (mult_single xs n ((n*x +carry)/10))
;; 

(*performs unsigned multiplication on 2 lists using Horners recursion*)
let rec mult_mag (l1:int list) (l2:int list) = 						   (* gives product of 2 int lists:takes l1 as normal, but l2 as reverse, returns the answer in a normal fashion with leading zeros*)
	match l2 with
	[] -> [0] |
	x::xs -> List.rev  (add_mag (mult_single (List.rev l1) x 0) (List.rev ((mult_mag l1 xs) @ [0])) 0)
;;



(*performs division on 2 int lists. ans is the quotient being formed in reverse. *)
let rec div_mag (l1:int list) (l2:int list) (curr:int list) (ans: int list) =	(*gives l2/l1; gives the answer in reverse*)
	if ( (gt_mag curr l1) || (curr=l1)) then
		div_mag l1 l2  (rem_zeros (List.rev (sub_mag (List.rev curr) (List.rev l1) 0)  ) )  (((List.hd ans)+1)::(List.tl ans))
    else
		if(List.length l2 = 0) then
			ans
		else
			div_mag l1 (List.tl l2) (curr@[List.hd l2]) (0::ans)
;;






(*converts an int list to a string*)
let rec itostring (l:int list) =										
	match l with
	[] -> "" |
	x::xs -> String.concat "" [string_of_int x;itostring xs]
;;


(*makes an int list out of a given unsigned integer*)
let rec mk_big_mag (n:int) =											(*converts given integer to int list*)
	if(n = 0) then 
		[]
	else 
		(mk_big_mag (n/10))@[n mod 10]
;;










(*required functions*)

(* Conversion functions from OCaml int to bigint. 0 is considered to be positive*)
let mk_big (a:int) : bigint =  
let s = if (a < 0) then Neg else NonNeg in
let l = 
if (a < 0) then 
	(mk_big_mag (-1*a))
else if( a = 0 ) then
	[0]
else
	(mk_big_mag a) 
in
(s,l)
;;


(*returns unary negation for a bigint*)
let minus (a:bigint) : bigint = match a with (sign,in_list) -> (rev_sign(sign), in_list);;


(*gives truth value for a=b. Corner case for +0 and -0 are handled*)
let eq (a:bigint) (b:bigint) : bool =
	let s1=match a with (sign, in_list) -> sign in
	let s2=match b with (sign, in_list) -> sign in
	let l1=match a with (sign, in_list) -> (rem_zeros in_list) in
	let l2=match b with (sign, in_list) -> (rem_zeros in_list) in
	if (s1=s2 && l1=l2) then
		true 
	else if ( l1=[0] && l2=[0] ) then
		true
	else
		false
;;




(*returns a+b*)
let add (a:bigint) (b:bigint) : bigint = 
	let s1=match a with (sign, in_list) -> sign in
	let s2=match b with (sign, in_list) -> sign in
	let l1=match a with (sign, in_list) -> (List.rev in_list) in
	let l2=match b with (sign, in_list) -> (List.rev in_list) in
	if( s1 = s2 ) then
		(s1, rem_zeros( List.rev (add_mag l1 l2 0) ) )
	else 
		if( (gt_mag (List.rev l1) (List.rev l2) ) ) then
			(s1, rem_zeros( List.rev (sub_mag l1 l2 0) ) )
		else
			(s2, rem_zeros( List.rev (sub_mag l2 l1 0) ) )
;;

(*returns a*b*)
let mult (a:bigint) (b:bigint) : bigint = 
	let s1=match a with (sign, in_list) -> sign in
	let s2=match b with (sign, in_list) -> sign in
	let l1=match a with (sign, in_list) -> in_list in
	let l2=match b with (sign, in_list) -> (List.rev in_list) in
	if (s1 = s2) then
		(NonNeg, rem_zeros ( mult_mag l1 l2 ))
	else
		(Neg,    rem_zeros(  mult_mag l1 l2 ))
;;


let sub (a:bigint) (b:bigint) : bigint = add a (minus b);;


(*returns a/b; exception for division by 0*)
let div (a:bigint) (b:bigint) : bigint = 			
	if (eq b (mk_big 0)) then
		raise Division_by_zero
	else
		let s1=match a with (sign, in_list) -> sign in
		let s2=match b with (sign, in_list) -> sign in
		let l1=match a with (sign, in_list) -> in_list in
		let l2=match b with (sign, in_list) -> in_list in
		if (s1 = s2) then
			(NonNeg, rem_zeros( List.rev (div_mag l2 l1 [] [0]) ))
		else
			(Neg,    rem_zeros( List.rev (div_mag l2 l1 [] [0]) ))
;;

(*returns absolute value of bigint*)
let abs (a:bigint) : bigint = match a with (sign,in_list)	-> (NonNeg,in_list);;

(*equivalent function of integer 'mod' in Ocaml for bigints; sign handling is done in the same way as ocaml does ; exception handling is done for 0*)
let rem (a:bigint) (b:bigint) : bigint = 
	if (eq b (mk_big 0)) then
		raise Division_by_zero
	else
		let s1=match a with (sign, in_list) -> sign in
		let abs_a = abs a in
		let abs_b = abs b in
		if ( s1 = NonNeg ) then
			sub abs_a (mult (div abs_a abs_b) abs_b)
		else
			minus (sub abs_a (mult (div abs_a abs_b) abs_b))
;;









(*return truth value for a>b*)
let gt (a:bigint) (b:bigint) :bool = match (a,b) with ((sign_a,in_list_a),(sign_b,in_list_b))
->  
	if (eq a b) then
		false
    else if(sign_a = Neg && sign_b = NonNeg) then 
		false
    else if(sign_a = NonNeg && sign_b = Neg) then
   		true
    else if(sign_a = Neg && sign_b = Neg) then
   		not(gt_mag in_list_a in_list_b)
    else
   		(gt_mag in_list_a in_list_b)
;;


(*Comparsion functions represented in form of gt and eq*)
let geq (a:bigint) (b:bigint) : bool = ((gt a b) || (eq a b)) ;;  
let lt (a:bigint) (b:bigint) : bool = not(geq a b) ;;
let leq a b = not(gt a b);;







(* Functions to present the result in the form of a string. Leading zeros are removed. *)
let print_num (a:bigint) : string = match a with 
 	(Neg,l) -> String.concat "" ["-";itostring (rem_zeros l)] |
 	 (_,l)		-> itostring l
;;







end
