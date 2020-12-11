use "hw1.sml";

val a1 = is_older ((2020, 6, 6), (2021, 6, 6))
val a2 = is_older ((2020, 6, 6), (2020, 7, 6))
val a3 = is_older ((2020, 6, 6), (2020, 6, 7))
val a4 = is_older ((2020, 6, 6), (2019, 6, 6))
val a5 = is_older ((2020, 6, 6), (2020, 5, 6))
val a6 = is_older ((2020, 6, 6), (2020, 6, 5))
val a7 = is_older ((2020, 6, 6), (2020, 6, 6))

val b1 = number_in_month ([], 1)
val b2 = number_in_month ([(2020, 1, 1), (2020, 2, 1)], 1)
val b3 = number_in_month ([(2020, 1, 1), (2020, 1, 1)], 1)
val b4 = number_in_month ([(2020, 1, 1), (2020, 1, 1)], 2)

val c1 = number_in_months ([], [])
val c2 = number_in_months ([(2020, 1, 1), (2020, 2, 1)], [])
val c3 = number_in_months ([(2020, 1, 1), (2020, 2, 1)], [3])
val c4 = number_in_months ([(2020, 1, 1), (2020, 2, 1)], [1])
val c5 = number_in_months ([(2020, 1, 1), (2020, 2, 1)], [1, 2])
val c6 = number_in_months ([(2020, 1, 1), (2020, 2, 1)], [1, 2, 3])

val d1 = dates_in_month ([], 1)
val d2 = dates_in_month ([(2020, 1, 1), (2020, 2, 1)], 3)
val d3 = dates_in_month ([(2020, 1, 1), (2020, 2, 1)], 1)
val d4 = dates_in_month ([(2020, 1, 1), (2020, 2, 1)], 2)
val d5 = dates_in_month ([(2020, 1, 1), (2020, 6, 6), (2020, 1, 2)], 1)

val e1 = dates_in_months ([], [])
val e2 = dates_in_months ([], [1])
val e3 = dates_in_months ([(2020, 1, 1), (2020, 2, 1)], [])
val e4 = dates_in_months ([(2020, 1, 1), (2020, 2, 1)], [3])
val e5 = dates_in_months ([(2020, 1, 1), (2020, 2, 1)], [1])
val e6 = dates_in_months ([(2020, 1, 1), (2020, 2, 1)], [1, 2])
val e7 = dates_in_months ([(2020, 1, 1), (2020, 6, 6), (2020, 1, 2)], [1, 2])

val f1 = get_nth (["a"], 1)
val f2 = get_nth (["b"], 1)
val f3 = get_nth (["a", "b"], 1)
val f4 = get_nth (["a", "b"], 2)
val f5 = get_nth (["a", "b", "c"], 2)

val g1 = date_to_string (2020, 2, 3)
val g2 = date_to_string (2019, 6, 30)

val h1 = number_before_reaching_sum (1, [1, 2, 3])
val h2 = number_before_reaching_sum (3, [1, 2, 3])
val h3 = number_before_reaching_sum (4, [1, 2, 3])
val h4 = number_before_reaching_sum (6, [1, 2, 3])
val h5 = number_before_reaching_sum (7, [1, 2, 3])
val h6 = number_before_reaching_sum (5, [3, 1, 2])
val h7 = number_before_reaching_sum (5, [3, 2, 2])
val h8 = number_before_reaching_sum (6, [4, 1, 1, 1])
val h9 = number_before_reaching_sum (10, [1, 2, 3, 4, 5])

val i1 = what_month 1
val i2 = what_month 31
val i3 = what_month 32
val i4 = what_month 59
val i5 = what_month 60
val i6 = what_month 365
 
val j1 = month_range (30, 31)
val j2 = month_range (30, 32)
val j3 = month_range (58, 60)
val j4 = month_range (60, 58)
 
val k1 = oldest []
val k2 = oldest [(2020, 1, 1)]
val k3 = oldest [(2021, 1, 1), (2020, 1, 1), (2022, 1, 1)]

val l1 = exists (1, [])
val l2 = exists (1, [1])
val l3 = exists (1, [3, 2])
val l4 = exists (1, [3, 2, 1])

val m1 = dedupe ([])
val m2 = dedupe ([1])
val m3 = dedupe ([1, 1])
val m4 = dedupe ([1, 2, 1])
val m5 = dedupe ([1, 2, 2, 1])

val n1 = number_in_months_challenge ([], [])
val n2 = number_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [])
val n3 = number_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [3])
val n4 = number_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1])
val n5 = number_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1, 1, 2])
val n6 = number_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1, 2, 3, 2])

val o1 = dates_in_months_challenge ([], [])
val o2 = dates_in_months_challenge ([], [1])
val o3 = dates_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [])
val o4 = dates_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [3])
val o5 = dates_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1])
val o6 = dates_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1, 1])
val o7 = dates_in_months_challenge ([(2020, 1, 1), (2020, 2, 1)], [1, 2, 1])
val o8 = dates_in_months_challenge ([(2020, 1, 1), (2020, 6, 6), (2020, 1, 2)], [1, 2, 1])

val p1 = reasonable_date (2020, 1, 1)
val p2 = reasonable_date (2020, 1, 31)
val p3 = reasonable_date (2020, 2, 28)
val p4 = reasonable_date (2020, 6, 30)
val p5 = reasonable_date (2020, 12, 31)
val p6 = reasonable_date (1, 1, 1)
val p7 = reasonable_date (2020, 2, 29)
val p8 = reasonable_date (2000, 2, 29)
val p9 = reasonable_date (0, 1, 1)
val p10 = reasonable_date (2020, 0, 1)
val p11 = reasonable_date (2020, 13, 1)
val p12 = reasonable_date (2020, 1, 0)
val p13 = reasonable_date (2020, 1, 32)
val p14 = reasonable_date (2021, 2, 29)
