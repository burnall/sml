(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(except, xs) =
    case xs of
        [] => NONE
      | x :: xs' =>
            if same_string(x, except)
            then SOME xs' 
            else 
                case all_except_option(except, xs') of
                    NONE => NONE
                 |  SOME xs'' => SOME(x :: xs'')

fun get_substitutions1(xss, tag) =
    case xss of 
        [] => []
      | xs :: xss' =>
           case (all_except_option(tag, xs), get_substitutions1(xss', tag)) of
               (NONE, xs') => xs'
             | (SOME(xs'), xs'') => xs' @ xs''  

fun get_substitutions2(xss, tag) =
    let 
        fun helper(acc, xss) =
            case xss of 
                [] => acc
              | xs :: xss' => 
                    case all_except_option(tag, xs) of 
                        NONE => helper(acc, xss')
                      | SOME(xs') => helper(acc @ xs', xss') 
    in
        helper([], xss)
    end  

fun similar_names(xss, {first, middle, last}) =
    let
        fun helper(acc, xs) =
            case xs of
                [] => acc
              | x :: xs' => helper({first=x, middle=middle, last=last} :: acc, xs')
    in
        helper([{first=first, middle=middle, last=last}], get_substitutions2(xss, first))
    end      
        
       
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card) =
    case card of
         (Clubs, _) => Black
       | (Spades, _) => Black
       | _ => Red 

fun card_value(card) =
    case card of
        (_, Num i) => i
      | (_, Ace) => 11
      |  _ => 10 

fun remove_card(cards, card_to_remove, exc) =
    case cards of
        [] => raise exc
      | card :: cards' => 
            if card = card_to_remove
            then cards'
            else card :: remove_card(cards', card_to_remove, exc)   

fun all_same_color(cards) = 
    case cards of 
        [] => true
     |  _ :: [] => true 
     |  c0 :: c1 :: cards' => 
            card_color(c0) = card_color(c1) andalso all_same_color(c1 :: cards')  

fun sum_cards(cards) =
    let
        fun helper(cards, acc) =
            case cards of 
                [] => acc
             |  card :: cards' => helper(cards', acc + card_value(card))
    in
        helper(cards, 0)
    end

(*
The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator).
*)
fun score(cards, goal) =
    let 
        val sum = sum_cards(cards)
        val preliminary = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color(cards)
        then preliminary div 2
        else preliminary
    end 
               
(*
Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list.
*)

fun officiate(cards, moves, goal) =
    let
        fun helper(cards, held_cards, moves) =
            case (moves, cards) of
                ([], _) => score(held_cards, goal)
             |  (Draw :: _, []) => score(held_cards, goal) 
             |  (Draw :: moves', card :: cards') =>
                    if sum_cards(card :: held_cards) > goal
                    then score(card :: held_cards, goal)
                    else helper(cards', card :: held_cards, moves')
             |  (Discard card :: moves', _) => helper(cards, remove_card(held_cards, card, IllegalMove), moves')  
    in
        helper(cards, [], moves)
    end
