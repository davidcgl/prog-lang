(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val a1 = all_except_option ("a", []) = NONE
val a2 = all_except_option ("a", ["a"]) = SOME []
val a3 = all_except_option ("a", ["a", "b"]) = SOME ["b"]
val a4 = all_except_option ("b", ["a", "b"]) = SOME ["a"]
val a5 = all_except_option ("b", ["a", "b", "c"]) = SOME ["a", "c"]
val a6 = all_except_option ("c", ["a", "b"]) = NONE

val b1 = get_substitutions1 ([["foo"]], "foo") = []
val b2 = get_substitutions1 ([["bar"]], "foo") = []
val b3 = get_substitutions1 ([["foo", "foo1"]], "foo") = ["foo1"]
val b4 = get_substitutions1 ([["foo", "foo1", "foo2"]], "foo") = ["foo1", "foo2"]
val b5 = get_substitutions1 ([["foo", "foo1"], ["foo", "foo2"]], "foo") = ["foo1", "foo2"]
val b6 = get_substitutions1 ([["foo", "foo1"], ["bar"], ["foo", "foo2"]], "foo") = ["foo1", "foo2"]
val b7 = get_substitutions1 ([["foo", "foo1"], ["foo", "foo1"]], "foo") = ["foo1", "foo1"]

val c1 = get_substitutions2 ([["foo"]], "foo") = []
val c2 = get_substitutions2 ([["bar"]], "foo") = []
val c3 = get_substitutions2 ([["foo", "foo1"]], "foo") = ["foo1"]
val c4 = get_substitutions2 ([["foo", "foo1", "foo2"]], "foo") = ["foo1", "foo2"]
val c5 = get_substitutions2 ([["foo", "foo1"], ["foo", "foo2"]], "foo") = ["foo1", "foo2"]
val c6 = get_substitutions2 ([["foo", "foo1"], ["bar"]], "foo") = ["foo1"]
val c7 = get_substitutions2 ([["foo", "foo1"], ["foo", "foo1"]], "foo") = ["foo1", "foo1"]

val name = {first="foo", middle="bar", last="baz"}
val d1 = similar_names ([["foo"]], name) =
  [{first="foo", middle="bar", last="baz"}]
val d2 = similar_names ([["bar"]], name) =
  [{first="foo", middle="bar", last="baz"}]
val d3 = similar_names ([["foo", "foo1"]], name) =
  [{first="foo", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"}]
val d4 = similar_names ([["foo", "foo1", "foo2"]], name) =
  [{first="foo", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"},
   {first="foo2", middle="bar", last="baz"}]
val d5 = similar_names ([["foo", "foo1"], ["foo", "foo2"]], name) =
  [{first="foo", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"},
   {first="foo2", middle="bar", last="baz"}]
val d6 = similar_names ([["foo", "foo1"], ["bar"], ["foo", "foo2"]], name) =
  [{first="foo", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"},
   {first="foo2", middle="bar", last="baz"}]
val d7 = similar_names ([["foo", "foo1"], ["bar"], ["foo", "foo1"]], name) =
  [{first="foo", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"},
   {first="foo1", middle="bar", last="baz"}]
   
val e1 = card_color (Spades, Num 2) = Black
val e2 = card_color (Hearts, Queen) = Red
val e3 = card_color (Clubs, Num 10) = Black
val e4 = card_color (Diamonds, Ace) = Red

val f1 = card_value (Spades, Num 1) = 1
val f2 = card_value (Hearts, Num 2) = 2
val f3 = card_value (Hearts, Num 10) = 10
val f4 = card_value (Clubs, Jack) = 10
val f5 = card_value (Clubs, Queen) = 10
val f6 = card_value (Clubs, King) = 10
val f7 = card_value (Clubs, Ace) = 11

val g1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val g2 = remove_card ([(Hearts, Ace), (Clubs, Jack)], (Hearts, Ace), IllegalMove) = [(Clubs, Jack)]
val g3 = remove_card ([(Hearts, Ace), (Clubs, Jack), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Jack), (Hearts, Ace)]
val g4 = (remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []
val g5 = (remove_card ([(Clubs, Ace)], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []

val h1 = all_same_color [] = true
val h2 = all_same_color [(Clubs, Ace)] = true
val h3 = all_same_color [(Clubs, Ace), (Clubs, Ace)] = true
val h4 = all_same_color [(Clubs, Ace), (Clubs, Jack)] = true
val h5 = all_same_color [(Clubs, Ace), (Spades, Ace)] = true
val h6 = all_same_color [(Clubs, Ace), (Hearts, Ace), (Clubs, Ace)] = false
val h7 = all_same_color [(Clubs, Ace), (Spades, Ace), (Diamonds, Ace)] = false

val i1 = sum_cards [] = 0
val i2 = sum_cards [(Clubs, Num 2)] = 2
val i3 = sum_cards [(Clubs, Num 2), (Clubs, Num 4)] = 6
val i4 = sum_cards [(Clubs, Num 2), (Hearts, Num 4)] = 6
val i5 = sum_cards [(Clubs, Num 2), (Hearts, Jack)] = 12
val i6 = sum_cards [(Clubs, Num 2), (Hearts, Ace)] = 13

val j1 = score ([], 0) = 0
val j2 = score ([], 10) = 5
val j3 = score ([(Hearts, Num 5)], 10) = 2
val j4 = score ([(Hearts, Num 5), (Spades, Num 3)], 10) = 2
val j5 = score ([(Hearts, Num 5), (Hearts, Num 3)], 10) = 1
val j6 = score ([(Hearts, Num 5), (Spades, Num 5)], 10) = 0
val j7 = score ([(Hearts, Num 5), (Clubs, Num 6)], 10) = 3
val j8 = score ([(Hearts, Num 5), (Hearts, Num 6)], 10) = 1

val k1 = officiate ([(Hearts, Num 2), (Clubs, Num 4)], [Draw], 15) = 6

val k2 = officiate ([(Clubs, Ace), (Spades,Ace), (Clubs,Ace), (Spades,Ace)],
                    [Draw, Draw, Draw, Draw, Draw],
                    42) = 3

val k3 = (officiate ([(Clubs, Jack), (Spades, Num(8))],
                     [Draw, Discard(Hearts, Jack)],
                     42) handle IllegalMove => ~1) = ~1