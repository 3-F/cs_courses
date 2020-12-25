(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, xs) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(s, x)
		  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE => NONE
			 | SOME lst => SOME (x::lst);
    
(*string list list * string -> string list*)
fun get_substitutions1 (xs, s) =
    case xs of
	[] => []
      | x::xs' => let val ans_tl = get_substitutions1(xs', s)
		  in
		      case all_except_option(s, x) of
			  NONE => ans_tl
			| SOME lst => lst @ ans_tl
		  end;
    
fun get_substitutions2 (xs, s) =
    let fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => case all_except_option(s, x) of
			      NONE => aux(xs', acc)
			    | SOME lst => aux(xs', lst @ acc)
    in
	aux(xs, [])
    end;

(*
(*sample 1-c*)
fun get_substitutions2 (xs, s) =
    let fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs', (case all_except_option(s, x) of
				   NONE => acc
				     | SOME y => acc @ y))
    in
	aux(xs, [])
    end;
*)

fun similar_names (xs, name) =
    let val {first=a, middle=b, last=c} = name
	fun similar_names_in_strlst xs =
	    case xs of
		[] => name :: []
	      | x::xs' => {first=x, middle=b, last=c} :: similar_names_in_strlst xs'
    in
	similar_names_in_strlst (get_substitutions2(xs, a))
    end;
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*
fun card_color card =
    let val (a, b) = card
    in
	case a of
	    Spades => Black
	  | Clubs => Black
	  | _ => Red
    end;
*)

(*sample 2-a*)
fun card_color card =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red; 

(*
fun card_value card =
    let val (a, b) = card
    in
	case b of
	    Num i => i
	  | Ace => 11
	  | _ => 10
    end;
*)

(*sample 2-b*)
fun card_value card =
    case card of
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10;
		      
fun remove_card (cs, c, e) =
    case cs of
	[] => []
      | x::xs => if x=c
		 then xs
		 else let val tl_lst = remove_card(xs, c, e)
		      in
			  if xs=tl_lst
			  then raise e
			  else x::tl_lst
		      end;

(*do it again*)
fun all_same_color cs =
    case cs of
	[] => true
      | x::xs => case xs of
			 [] => true
		       | y::ys => if card_color x = card_color y then all_same_color xs else false;

fun sum_cards cs =
    let fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs', card_value x + acc)
    in
	aux(cs, 0)
    end;

fun score (held_cards, goal) =
    let val sum_h = sum_cards held_cards
	val preliminary_score = if sum_h > goal then 3 * (sum_h - goal) else goal - sum_h
    in
	if all_same_color held_cards then preliminary_score div 2 else preliminary_score
    end;


fun officiate (card_list, move_list, goal) =
    let
	fun current_state (card_list, move_list, held_cards) =
	    let val s = score (held_cards, goal)
	    in
		if goal < sum_cards held_cards
		then s
		else case move_list of
			 [] => s
		       | x::xs => case x of
				  Draw => (case card_list of
					      [] => s
					    | y::ys => current_state(ys, xs, y::held_cards))
			        | Discard c => current_state(card_list, xs, remove_card (held_cards, c, IllegalMove))
	    end
    in
	current_state(card_list, move_list, [])
    end;

fun score_challenge (held_cards, goal) =
    let
	fun card_value card =
	    let val (a, b) = card
	    in
		case b of
		    Num i => i
		  | Ace => 1
		  | _ => 10
	    end
	fun sum_cards cs =
	    let fun aux (xs, acc) =
		    case xs of
			[] => acc
		      | x::xs' => aux(xs', card_value x + acc)
	    in
		aux(cs, 0)
	    end
	val sum_h = sum_cards held_cards
	val preliminary_score = if sum_h > goal then 3 * (sum_h - goal) else goal - sum_h
	val score = score(held_cards, goal)
	val score1 = if all_same_color held_cards then preliminary_score div 2 else preliminary_score 											
    in
	if score1 < score then score1 else score
    end;

fun officiate_challenge (card_list, move_list, goal) =
    let
	fun current_state (card_list, move_list, held_cards) =
	    let val s = score_challenge (held_cards, goal)
	    in
		if goal < sum_cards held_cards
		then SOME s
		else case move_list of
			 [] => SOME s
		       | x::xs => case x of
				  Draw => (case card_list of
					      [] => SOME s
					    | y::ys => current_state(ys, xs, y::held_cards))
			        | Discard c => current_state(card_list, xs, remove_card (held_cards, c, IllegalMove))
	    end
    in
	case current_state(card_list, move_list, []) of
	    SOME s => s
    end;	

(*A little hard*)
fun careful_player (card_list, goal) =
    let
        fun careful_moves(card_list, helds, moves) =
            let 
                fun remove_reach_zero (cards, goal) =
                    case cards of
                        [] => NONE
                      | c::cs => if sum_cards(remove_card (cards, c, IllegalMove)) = goal then SOME c
                                 else remove_reach_zero (cs, goal - card_value c)
            in
                case card_list of
                    [] => moves
                  | c::cs =>
                        if goal - sum_cards helds > 10
                        then careful_moves (cs, c::helds, moves @ [Draw])
                        else if goal = sum_cards helds
                        then moves
                        else case remove_reach_zero (c::helds, goal) of
                            NONE => if sum_cards (c::helds) > goal then moves
                                    else careful_moves(cs, c::helds, moves @ [Draw])
                          | SOME cd => moves @ [Discard cd, Draw]
            end
    in
        careful_moves(card_list, [], [])
    end;
