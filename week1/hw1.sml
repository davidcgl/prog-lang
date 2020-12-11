(* You will write 11 SML functions (and tests for them) related to calendar
 * dates. In all problems, a “date” is an SML value of type int*int*int, where
 * the first part is the year, the second part is the month, and the third part
 * is the day. A “reasonable” date has a positive year, a month between 1 and
 * 12, and a day no greater than 31 (or less depending on the month). Your
 * solutions need to work correctly only for reasonable dates, but do not check
 * for reasonable dates (that is a challenge problem) and many of your functions
 * will naturally work correctly for some/all non-reasonable dates. A “day of
 * year” is a number from 1 to 365 where, for example, 33 represents
 * February 2. (We ignore leap years except in one challenge problem.) *)

 
(* 1. Write a function is_older that takes two dates and evaluates to true or
 * false. It evaluates to true if the first argument is a date that comes before
 * the second argument. (If the two dates are the same, the result is false.) *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
  let
    val y1 = #1 date1
    val y2 = #1 date2
    val m1 = #2 date1
    val m2 = #2 date2
    val d1 = #3 date1
    val d2 = #3 date2
  in
    if y1 < y2 then
      true
    else if y1 = y2 andalso m1 < m2 then
      true 
    else if y1 = y2 andalso m1 = m2 andalso d1 < d2 then
      true
    else
      false
  end 

(* 2. Write a function number_in_month that takes a list of dates and a month
 * (i.e., an int) and returns how many dates in the list are in the given
 * month. *)

fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates then
    0
  else
    (if #2 (hd dates) = month then 1 else 0) + number_in_month (tl dates, month)

(* 3. Write a function number_in_months that takes a list of dates and a list
 * of months (i.e., an int list) and returns the number of dates in the list of
 * dates that are in any of the months in the list of months. Assume the list of
 * months has no number repeated. Hint: Use your answer to the previous
 * problem. *)

fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months then
    0
  else
    number_in_month (dates, hd months) + number_in_months (dates, tl months)
  
(* 4. Write a function dates_in_month that takes a list of dates and a month
 * (i.e., an int) and returns a list holding the dates from the argument list of
 * dates that are in the month. The returned list should contain dates in the
 * order they were originally given. *)

fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates then
    []
  else
    let
      val date = hd dates
      val tl_dates = dates_in_month (tl dates, month)
    in
      if #2 date = month then date :: tl_dates else tl_dates
    end
    
(* 5. Write a function dates_in_months that takes a list of dates and a list of
 * months (i.e., an int list) and returns a list holding the dates from the
 * argument list of dates that are in any of the months in the list of months.
 * Assume the list of months has no number repeated. Hint: Use your answer to
 * the previous problem and SML’s list-append operator (@). *)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months then
    []
  else
    dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* 6. Write a function get_nth that takes a list of strings and an int n and
 * returns the nth element of the list where the head of the list is 1st. Do
 * not worry about the case where the list has too few elements: your function
 * may apply hd or tl to the empty list in this case, which is okay. *)

fun get_nth (list : string list, n : int) =
  if n <= 1 then hd list else get_nth (tl list, n - 1)
  
(* 7. Write a function date_to_string that takes a date and returns a string of
 * the form January 20, 2013 (for example). Use the operator ^ for concatenating
 * strings and the library function Int.toString for converting an int to a
 * string. For producing the month part, do not use a bunch of conditionals.
 * Instead, use a list holding 12 strings and your answer to the previous
 * problem. For consistency, put a comma following the day and use capitalized
 * English month names: January, February, March, April, May, June, July,
 * August, September, October, November, December. *)

fun date_to_string (date : (int * int * int)) =
  let
    val year = Int.toString (#1 date)
    val months = [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ]
    val month = get_nth (months, #2 date) 
    val day = Int.toString (#3 date)
  in
    month ^ " " ^ day ^ ", " ^ year
  end
  
(* 8. Write a function number_before_reaching_sum that takes an int called sum,
 * which you can assume is positive, and an int list, which you can assume
 * contains all positive numbers, and returns an int.  You should return an int
 * n such that the first n elements of the list add to less than sum, but the
 * first n + 1 elements of the list add to sum or more. Assume the entire list
 * sums to more than the passed in value; it is okay for an exception to occur
 * if this is not the case. *)

fun number_before_reaching_sum (sum : int, nums : int list) =
  if null nums orelse sum <= hd nums then
    0
  else
    1 + number_before_reaching_sum (sum - hd nums, tl nums)
  
(* 9. Write a function what_month that takes a day of year (i.e., an int between
 * 1 and 365) and returns what month that day is in (1 for January, 2 for
 * February, etc.). Use a list holding 12 integers and your answer to the
 * previous problem. *)

fun what_month (day : int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum (day, days_in_month)
  end

    
(* 10. Write a function month_range that takes two days of the year day1 and
 * day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2
 * is the month of day1+1, ..., and mn is the month of day day2. Note the result
 * will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range (day1 : int, day2 : int) = 
  if day1 > day2 then
    []
  else 
    what_month day1 :: month_range (day1 + 1, day2)
  
(* 11. Write a function oldest that takes a list of dates and evaluates to an
 * (int*int*int) option. It evaluates to NONE if the list has no dates and SOME
 * d if the date d is the oldest date in the list. *)

fun oldest (dates : (int * int * int) list) =
  if null dates then
    NONE
  else
    let
      val date = hd dates
      val res = oldest (tl dates)
    in
      if isSome res andalso is_older (valOf res, date) then
        res
      else
        SOME date
    end
  
(* 12. Challenge Problem: Write functions number_in_months_challenge and
 * dates_in_months_challenge that are like your solutions to problems 3 and 5
 * except having a month in the second argument multiple times has no more
 * effect than having it once. (Hint: Remove duplicates, then use previous
 * work.) *)
 
fun exists (num : int, nums : int list) =
  if null nums then
    false
  else if num = hd nums then
    true
  else
    exists (num, tl nums)

fun unique (nums : int list) =
  if null nums then
    []
  else
    let
      val num = hd nums
      val res = unique (tl nums)
    in
      if exists (num, res) then res else num :: res
    end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
  number_in_months (dates, unique months)

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
  dates_in_months (dates, unique months)
  
(* 13. Challenge Problem: Write a function reasonable_date that takes a date and
 * determines if it describes a real date in the common era. A “real date” has a
 * positive year (year 0 did not exist), a month between 1 and 12, and a day
 * appropriate for the month. Solutions should properly handle leap years. Leap
 * years are years that are either divisible by 400 or divisible by 4 but not
 * divisible by 100. (Do not worry about days possibly lost in the conversion
 * to the Gregorian calendar in the Late 1500s.) *)

fun reasonable_date (date : (int * int * int)) =
  let
    fun get_nth (list : int list, n : int) =
      if n <= 1 then hd list else get_nth (tl list, n - 1)
    val y = #1 date
    val m = #2 date
    val d = #3 date
    val is_leap = (y mod 400 = 0) orelse ((y mod 4 = 0) andalso (y mod 100 <> 0))
    val days_in_month = [31, (if is_leap then 29 else 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    if y <= 0 then
      false
    else if m < 1 orelse m > 12 then
      false
    else if d < 1 orelse d > get_nth (days_in_month, m) then
      false
    else
      true
end
