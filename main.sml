
(* task1 *)
(* <0 if a < b
0 if a == b
>0 if a > b *)
(* fun is_older(a:int*int*int, b:int*int*int) : int =
    if not((#1 a - #1 b) = 0)
    then (#1 a - #1 b)
    else if not((#2 a - #2 b) = 0)
    then (#2 a - #2 b)
    else (#3 a - #3 b)

val task1 = is_older((2002,3,29) , (2002,3,30)); *)

(* task2 *)
(* fun number_in_month_r ([], acc, month) = acc
  (* | sum (x::xs, acc) = sum (xs, x + acc) *)
  | number_in_month_r ((y,m,d)::xs, acc, month) = if (month = m) then number_in_month_r(xs, acc+1, month) else number_in_month_r(xs, acc, month)

fun number_in_month(date_list : (int*int*int) list, month : int) : int =
  number_in_month_r(date_list, 0, month)

val task2 = number_in_month([(2002,1,29) , (2002,1,30) , (2002,1,1)], 1); *)

(* task3 *)
(* fun date_r ([], acc, month) = acc
  | date_r ((y,m,d)::xs, acc, month) = if (month = m) then date_r(xs, acc+1, month) else date_r(xs, acc, month)

fun month_r (date_list, [], acc) = acc
  | month_r (date_list, x::xs, acc) = month_r(date_list, xs, date_r(date_list, acc, x))

fun number_in_month(date_list : (int*int*int) list, month_list : int list) : int =
  month_r(date_list, month_list, 0)

val task3 = number_in_month([(2002,1,29) , (2002,2,30) , (2002,3,1)], [1, 2, 5]); *)

(* task4 *)
(* fun dates_in_month_r ([], acc, month) = acc
  | dates_in_month_r ((y,m,d)::xs, acc, month) = if (month = m) then dates_in_month_r(xs, acc @ [(y,m,d)], month) else dates_in_month_r(xs, acc, month)

fun dates_in_month(date_list : (int*int*int) list, month : int) : (int*int*int) list =
  dates_in_month_r(date_list, [], month)

val task4 = dates_in_month([(2002,3,29) , (2002,2,30) , (2002,3,1)], 3); *)

(* task5 *)
(* fun date_r ([], acc, month) = acc
  | date_r ((y,m,d)::xs, acc, month) = if (month = m) then date_r(xs, acc @ [(y,m,d)], month) else date_r(xs, acc, month)

fun month_r (date_list, [], acc) = acc
  | month_r (date_list, x::xs, acc) = month_r(date_list, xs, date_r(date_list, acc, x))

fun dates_in_month(date_list : (int*int*int) list, month_list : int list) : (int*int*int) list =
  month_r(date_list, month_list, [])

val task5 = dates_in_month([(2002,2,29) , (2002,2,30) , (2002,2,1)], [1, 2, 5]); *)

(* task6 *)
(* fun get_nth_r ([] : string list, n, acc) = "Index out of range"
  | get_nth_r (x::xs : string list, n, acc) = if (acc = n) then x else get_nth_r(xs, n, acc+1)

fun get_nth(string_list : string list, n : int) : string =
  get_nth_r(string_list, n, 1)

val task6 = get_nth(["1","2","3","4","5","6","7","8","9","10","11"], 10); *)

(* task7 *)
(* fun date_to_string(date : (int*int*int)) : string = get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"], (#2 date))^" "^Int.toString(#3 date)^","^Int.toString(#1 date)

val task7 = date_to_string((2002,3,29)); *)

(* task8 *)
(* fun nbrs_r1 ([], acc) = acc
  | nbrs_r1 (x::xs, acc) = nbrs_r1(xs, acc+x)

fun nbrs_r ([], sum, n, acc) = 0
  | nbrs_r (xn::xn1::xs, sum, n, acc) = if (((acc + xn) < sum) andalso (sum <= nbrs_r1(xn1::xs, 0))) then n else nbrs_r(xn1::xs, sum, n+1, acc+xn)

fun number_before_reaching_sum(nums : int list, sum : int) : int =
  nbrs_r(nums, sum, 1, 0)

val task8 = number_before_reaching_sum([1,2,3,4,7,8,9], 7); *)
