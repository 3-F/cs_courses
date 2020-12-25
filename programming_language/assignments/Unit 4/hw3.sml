(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer
(*	      
fun only_capitals strs =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) strs

fun longest_string1 strs =
    List.foldl (fn (x2, x1) => if String.size x2 > String.size x1 then x2 else x1) "" strs

fun longest_string2 strs =
    List.foldl (fn (x2, x1) => if String.size x2 >= String.size x1 then x2 else x1) "" strs

*)

val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

val longest_string1 =
    List.foldl (fn (s, sofar) => if String.size s > String.size sofar
				 then s
				 else sofar)
	       ""

val longest_string2 =
    List.foldl (fn (s, sofar) => if String.size s >= String.size sofar
				 then s
				 else sofar)
	       ""
	       
fun longest_string_helper f strs = 
    List.foldl (fn (x2, x1) => if f(String.size x2, String.size x1) then x2 else x1) "" strs
	       
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals
				  
(* fun rev_string str =
    String.implode(List.rev(String.explode str)) *)

val rev_string = String.implode o rev o String.explode
					    
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v

fun all_answers f xs =
    let
	fun aux acc xs =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => aux (acc@v) xs'
    in
	aux [] xs
    end


(* Problem 9-12 *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** you can put all your code here ****)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(*
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)
*)

val count_wild_and_variable_lengths = g (fn () => 1) String.size
			
fun count_some_var (str, p) =
    g (fn () => 0) (fn s => if s=str then 1 else 0) p

fun check_pat p =
    let
	fun to_strs p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (x, acc) => to_strs x @ acc) [] ps
	      | ConstructorP(_, p) => to_strs p
	      | _ => []

	fun no_repeate strs =
	    case strs of
		[] => true
	      | x::xs' => (not (List.exists (fn x' => x' = x) xs')) andalso no_repeate xs'
    in
	no_repeate (to_strs p)
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Const c1, ConstP c2) => if c1 = c2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | (Constructor (s2, v'), ConstructorP (s1, p')) => if s1 = s2 then match(v', p') else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
	  
(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
