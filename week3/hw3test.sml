use "hw3.sml";

val a1 = only_capitals [] = []
val a2 = only_capitals ["a"] = []
val a3 = only_capitals ["A"] = ["A"]
val a4 = only_capitals ["A", "B"] = ["A", "B"]
val a5 = only_capitals ["A", "b", "C"] = ["A", "C"]

val b1 = longest_string1 [] = ""
val b2 = longest_string1 [""] = ""
val b3 = longest_string1 ["a"] = "a"
val b4 = longest_string1 ["a", "bb", "cc"] = "bb"
val b5 = longest_string1 ["aa", "b", "cc"] = "aa"

val c1 = longest_string2 [] = ""
val c2 = longest_string2 [""] = ""
val c3 = longest_string2 ["a"] = "a"
val c4 = longest_string2 ["a", "bb", "cc"] = "cc"
val c5 = longest_string2 ["aa", "bb", "c"] = "bb"

val d1 = longest_string3 [] = ""
val d2 = longest_string3 [""] = ""
val d3 = longest_string3 ["a"] = "a"
val d4 = longest_string3 ["a", "bb", "cc"] = "bb"
val d5 = longest_string3 ["aa", "b", "cc"] = "aa"

val e1 = longest_string4 [] = ""
val e2 = longest_string4 [""] = ""
val e3 = longest_string4 ["a"] = "a"
val e4 = longest_string4 ["a", "bb", "cc"] = "cc"
val e5 = longest_string4 ["aa", "bb", "c"] = "bb"

val f1 = longest_capitalized [] = ""
val f2 = longest_capitalized ["a"] = ""
val f3 = longest_capitalized ["aa", "bb"] = ""
val f4 = longest_capitalized ["aa", "Bb"] = "Bb"
val f5 = longest_capitalized ["Aa", "Bb"] = "Aa"
val f6 = longest_capitalized ["Aa", "Bbb"] = "Bbb"

val g1 = rev_string "" = ""
val g2 = rev_string "a" = "a"
val g3 = rev_string "ab" = "ba"
val g4 = rev_string "abc" = "cba"

val h1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4, 5] = 4
val h2 = first_answer (fn x => if x < 3 then SOME x else NONE) [1, 2, 3, 4, 5] = 1
val h3 = first_answer (fn x => if String.size x > 1 then SOME x else NONE) ["a", "bb", "cc"] = "bb"

val i1 = all_answers (fn x => SOME [x]) [] = SOME []
val i2 = all_answers (fn x => NONE) [1] = NONE
val i3 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [1, 2, 3] = NONE
val i4 = all_answers (fn x => if x > 0 then SOME [x] else NONE) [1, 2, 3] = SOME [1, 2, 3]
val i5 = all_answers (fn x => if String.size x > 0 then SOME [x] else NONE) ["a", "b"] = SOME ["a", "b"]

val j1 = count_wildcards UnitP = 0
val j2 = count_wildcards (Variable("x")) = 0
val j3 = count_wildcards (ConstP(1)) = 0
val j4 = count_wildcards Wildcard = 1
val j5 = count_wildcards (TupleP [Wildcard]) = 1
val j6 = count_wildcards (TupleP [Wildcard, UnitP, Wildcard]) = 2
val j7 = count_wildcards (TupleP [TupleP [Wildcard, UnitP], Wildcard]) = 2

val k1 = count_some_var ("x", Variable "y") = 0
val k2 = count_some_var ("x", Variable "x") = 1
val k3 = count_some_var ("x", TupleP [Variable "x"]) = 1
val k4 = count_some_var ("x", TupleP [Variable "x", Variable "y"]) = 1
val k5 = count_some_var ("x", TupleP [Variable "x", Variable "x"]) = 2
val k6 = count_some_var ("x", TupleP [Variable "x", Wildcard]) = 1

val l1 = check_pat UnitP = true
val l2 = check_pat (Variable "x") = true
val l3 = check_pat (TupleP [Variable "x"]) = true
val l4 = check_pat (TupleP [Variable "x", UnitP]) = true
val l5 = check_pat (TupleP [Variable "x", Variable "y"]) = true
val l6 = check_pat (TupleP [Variable "x", Variable "x"]) = false
val l7 = check_pat (TupleP [Variable "x", UnitP, Variable "x"]) = false
val l8 = check_pat (ConstructorP ("a", TupleP [Variable "x", Variable "x"])) = false
val l9 = check_pat (ConstructorP ("a", TupleP [Variable "x", ConstructorP ("b", Variable "x")])) = false
val l10 = check_pat (ConstructorP ("a", TupleP [Variable "x", ConstructorP ("b", TupleP [Variable "x"])])) = false

val m1 = match (Const 1, Wildcard) = SOME []
val m2 = match (Unit, Wildcard) = SOME []
val m3 = match (Tuple [Const 1], Wildcard) = SOME []

val m4 = match (Const 1, Variable "x") = SOME [("x", Const 1)]
val m5 = match (Unit, Variable "y") = SOME [("y", Unit)]

val m6 = match (Const 1, UnitP) = NONE
val m7 = match (Unit, UnitP) = SOME []

val m8 = match (Const 1, ConstP 0) = NONE
val m9 = match (Const 1, ConstP 1) = SOME []

val m10 = match (Const 1, TupleP [ConstP 1]) = NONE
val m11 = match (Tuple [Const 0], TupleP [ConstP 1]) = NONE
val m12 = match (Tuple [Const 1], TupleP [ConstP 1]) = SOME []
val m13 = match (Tuple [Const 1], TupleP [Variable "x"]) = SOME [("x", Const 1)]
val m14 = match (Tuple [Const 1, Const 2], TupleP [Variable "x", ConstP 2]) = SOME [("x", Const 1)]

val n1 = first_match Unit [ConstP 1] = NONE
val n2 = first_match Unit [UnitP] = SOME []
val n3 = first_match Unit [ConstP 1, UnitP] = SOME []
val n4 = first_match (Const 1) [ConstP 1, UnitP] = SOME []
val n5 = first_match (Const 1) [Variable "x", UnitP] = SOME [("x", Const 1)]