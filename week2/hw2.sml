(* *****************************************************************************
 * Part 1
 *
 * This problem involves using first-name substitutions to come up with
 * alternate names. For example, Fredrick William Smith could also be Fred
 * William Smith or Freddie William Smith. Only part (d) is specifically about
 * this, but the other problems are helpful. *)

fun same_string (s1 : string, s2 : string) = s1 = s2
  
(* a. Write a function all_except_option, which takes a string and a string
 * list. Return NONE if the string is not in the list, else return SOME lst
 * where lst is identical to the argument list except the string is not in it.
 * You may assume the string is in the list at most once. Use same_string,
 * provided to you, to compare strings. Sample solution is around 8 lines. *)

fun all_except_option (name : string, names : string list) =
  case names of
      [] => NONE
    | x :: xs =>
      if same_string (x, name) then
        SOME xs
      else
        case all_except_option (name, xs) of
            NONE => NONE
          | SOME lst => SOME (x :: lst)


(* b. Write a function get_substitutions1, which takes a string list list (a
 * list of list of strings, the substitutions) and a string s and returns a
 * string list. The result has all the strings that are in some list in
 * substitutions that also has s, but s itself should not be in the result.
 * 
 * Example:
 *   get_substitutions1([["Fred", "Fredrick"],
 *                       ["Elizabeth", "Betty"],
 *                       ["Freddie", "Fred", "F"]],
 *                      "Fred")
 *
 * Answer: ["Fredrick", "Freddie", "F"]
 * 
 * Assume each list in substitutions has no repeats. The result will have
 * repeats if s and another string are both in more than one list in
 * substitutions.
 * 
 * Example:
 *   get_substitutions1([["Fred", "Fredrick"],
 *                       ["Jeff", "Jeffrey"],
 *                       ["Geoff", "Jeff", "Jeffrey"]],
 *                      "Jeff")
 *
 * Answer : ["Jeffrey", "Geoff", "Jeffrey"]
 * 
 * Use part (a) and ML’s list-append (@) but no other helper functions. Sample
 * solution is around 6 lines. *)

fun get_substitutions1 (subs : string list list, name : string) =
  case subs of
      [] => []
    | x :: xs =>
      case all_except_option (name, x) of
          NONE => get_substitutions1 (xs, name)
        | SOME lst => lst @ get_substitutions1 (xs, name)

(* c. Write a function get_substitutions2, which is like get_substitutions1
 * except it uses a tail-recursive local helper function. *)

fun get_substitutions2 (subs : string list list, name : string) =
  let
    fun get_subs (subs : string list list, acc : string list) =
      case subs of
          [] => acc
        | x :: xs =>
          case all_except_option (name, x) of
              NONE => get_subs (xs, acc)
            | SOME lst => get_subs (xs, acc @ lst)
  in
    get_subs (subs, [])
  end

(* d. Write a function similar_names, which takes a string list list of
 * substitutions (as in parts (b) and (c)) and a full name of type
 * {first:string,middle:string,last:string} and returns a list of full names
 * (type {first:string,middle:string,last:string} list). The result is all the
 * full names you can produce by substituting for the first name (and only the
 * first name) using substitutions and parts (b) or (c). The answer should
 * begin with the original name (then have 0 or more other names).
 *
 * Example:
 *   similar_names([["Fred", "Fredrick"],
 *                  ["Elizabeth", "Betty"],
 *                  ["Freddie", "Fred", "F"]],
 *                 {first="Fred", middle="W", last="Smith"})
 * 
 * Answer:
 *   [{first="Fred", last="Smith", middle="W"},
 *    {first="Fredrick", last="Smith", middle="W"},
 *    {first="Freddie", last="Smith", middle="W"},
 *    {first="F", last="Smith", middle="W"}]
 *   
 * Do not eliminate duplicates from the answer. Hint: Use a local helper
 * function. Sample solution is around 10 lines. *)

fun similar_names (subs : string list list,
                   name : {first : string, middle : string, last : string}) =
  let
    val {first=first, middle=middle, last=last} = name
    fun get_subs [] = []
      | get_subs (x :: xs) = {first=x, middle=middle, last=last} :: (get_subs xs)
  in
    name :: get_subs (get_substitutions1 (subs, first))
  end
  
(* *****************************************************************************
 * Part 2
 *
 * This problem involves a solitaire card game invented just for this question.
 * You will write a program that tracks the progress of a game; writing a game
 * player is a challenge problem. You can do parts (a)–(e) before understanding
 * the game if you wish. A game is played with a card-list and a goal. The
 * player has a list of held-cards, initially empty. The player makes a move
 * by either drawing, which means removing the first card in the card-list from
 * the card-list and adding it to the held-cards, or discarding, which means
 * choosing one of the held-cards to remove. The game ends either when the
 * player chooses to make no more moves or when the sum of the values of the
 * held-cards is greater than the goal. The objective is to end the game with
 * a low score (0 is best). Scoring works as follows: Let sum be the sum of the
 * values of the held-cards. If sum is greater than goal, the preliminary score
 * is three times (sum−goal), else the preliminary score is (goal − sum). The
 * score is the preliminary score unless all the held-cards are the same color,
 * in which case the score is the preliminary score divided by 2 (and rounded
 * down as usual with integer division; use ML’s div operator). *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* a. Write a function card_color, which takes a card and returns its color
 * (spades and clubs are black, diamonds and hearts are red). Note: One
 * case-expression is enough. *)

fun card_color (Spades, _) = Black
  | card_color (Hearts, _) = Red
  | card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red

(* b. Write a function card_value, which takes a card and returns its value
 * (numbered cards have their number as the value, aces are 11, everything else
 * is 10).  Note: One case-expression is enough. *)

fun card_value (_, Num n) = n
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10
  
(* c. Write a function remove_card, which takes a list of cards cs, a card c,
 * and an exception e. It returns a list that has all the elements of cs except
 * c. If c is in the list more than once, remove only the first one. If c is
 * not in the list, raise the exception e. You can compare cards with =. *)

fun remove_card (cards : card list, card : card, e : exn) =
  let
    fun remove_one [] = raise e
      | remove_one (x :: xs) = if card = x then xs else x :: (remove_one xs)
  in
    remove_one cards
  end
  

(* d. Write a function all_same_color, which takes a list of cards and returns
 * true if all the cards in the list are the same color. Hint: An elegant
 * solution is very similar to one of the functions using nested
 * pattern-matching in the lectures. *)

fun all_same_color [] = true
  | all_same_color (x :: []) = true
  | all_same_color (x1 :: x2 :: xs) =
    (card_color x1) = (card_color x2) andalso all_same_color (x2 :: xs)

  
(* e. Write a function sum_cards, which takes a list of cards and returns the
 * sum of their values. Use a locally defined helper function that is tail
 * recursive. (Take “calls use a constant amount of stack space” as a
 * requirement for this problem.) *)

fun sum_cards (cards : card list) =
  let
    fun sum ([], acc) = acc
      | sum (x :: xs, acc) = sum (xs, acc + card_value x)
  in
    sum (cards, 0)
  end
  
(* f. Write a function score, which takes a card list (the held-cards) and an
 * int (the goal) and computes the score as described above. *)

fun score (cards : card list, goal : int) =
  let
    val sum = sum_cards cards
    val prelim = if sum > goal then 3 * (sum - goal) else goal - sum
  in
    if all_same_color cards then prelim div 2 else prelim
  end

(* g. Write a function officiate, which "runs a game." It takes a card list
 * (the card-list) a move list (what the player "does" at each point), and an
 * int (the goal) and returns the score at the end of the game after processing
 * (some or all of) the moves in the move list in order. Use a locally defined
 * recursive helper function that takes several arguments that together
 * represent the current state of the game. As described above:
 *
 * - The game starts with the held-cards being the empty list. 
 * - The game ends if there are no more moves. (The player chose to stop since
 *   the move list is empty.)
 * - If the player discards some card c, play continues (i.e., make a recursive
 *   call) with the held-cards not having c and the card-list unchanged. If c is
 *   not in the held-cards, raise the IllegalMove exception.
 * - If the player draws and the card-list is (already) empty, the game is over.
 *   Else if drawing causes the sum of the held-cards to exceed the goal, the
 *   game is over (after drawing). Else play continues with a larger held-cards
 *   and a smaller card-list.
 *
 * Sample solution for (g) is under 20 lines. *)

fun officiate (cards : card list, moves : move list, goal : int) =
  let
    fun run (cards, moves, held) =
      if (sum_cards held) > goal then
        score (held, goal)
      else
        case moves of
            [] => score (held, goal)
          | Discard c :: ms =>
            run (cards, ms, remove_card (held, c, IllegalMove))
          | Draw :: ms =>
            case cards of
                [] => score (held, goal)
              | c :: cs => run (cs, ms, c :: held)
  in
    run (cards, moves, [])
  end

