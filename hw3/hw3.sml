(* Coursera Programming Languages, Homework 3, Provided Code *)

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
    #2 (foldl (fn (x, (len, s)) => 
        let 
            val cur_len = String.size(x) 
        in
            if cur_len > len
            then (cur_len, x)
            else (len, s)
        end) (0, "") xs)

fun longest_string2 xs =
    #2 (foldl (fn (x, (len, s)) => 
        let 
            val cur_len = String.size(x) 
        in
            if cur_len >= len
            then (cur_len, x)
            else (len, s)
        end) (0, "") xs)

fun longest_string_helper f xs =
  #2 (foldl (fn (x, (len, s)) => 
      let 
          val cur_len = String.size(x) 
      in
          if f(cur_len, len)
          then (cur_len, x)
          else (len, s)
      end) (0, "") xs)

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode 

exception NoAnswer

fun first_answer f xs =
    case List.find (isSome o f) xs of
        NONE => raise NoAnswer
     | SOME v => v 

fun all_answers f xs = 
    let
       fun helper(x, acc) = case (f x, acc) of
           (_, NONE) => NONE
         | (NONE, _) => NONE
         | (SOME v, SOME xs) => SOME (xs @ v) 
             
    in
        List.foldl helper (SOME []) xs
    end


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
	    Wildcard => f1 ()
	  | Variable x => f2 x
	  | TupleP ps => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _ => 0
    end

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (v, p) = 
    g (fn () => 0) (fn x => if v = x then 1 else 0) p  

val check_pat = let
        fun scan_variables p = 
	    case p of
 	        Variable x => [x]
	        | TupleP ps => List.foldl (fn (p, vs) => vs @ (scan_variables p)) [] ps
	        | ConstructorP(_, p) => scan_variables p
	        | _ => []
        fun all_distinct (xs: string list) = 
            #1 (foldl
                (fn (s, (ok, xs)) => (ok andalso not (List.exists (fn i => i = s) xs), s ::xs))
                (true, [])
                xs) 
    in
        all_distinct o scan_variables 
    end


(*
 Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines. Remember to look above for the
rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier.


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
*)
fun match(v, p) = 
    case (v, p) of
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)] 
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Unit, UnitP) => SOME []
      | (Tuple vs, TupleP ps) => 
            if List.length(vs) = List.length(ps) 
            then all_answers match (ListPair.zip(vs, ps))
            else NONE         
      | (Constructor (s1, v'), ConstructorP (s2, p')) => if s1 = s2 then match(v', p') else NONE
      | _  => NONE   


(*
Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
handle-expression. Hints: Sample solution is 3 lines.
*)
fun first_match () = SOME []
