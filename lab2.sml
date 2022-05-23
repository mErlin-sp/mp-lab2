
(* task1 *)
(* <0 if a < b
0 if a == b
>0 if a > b *)
fun is_older(a : (int*int*int), b : (int*int*int)) : int =
    if not((#1 a - #1 b) = 0)
    then (#1 a - #1 b)
    else if not((#2 a - #2 b) = 0)
    then (#2 a - #2 b)
    else (#3 a - #3 b)

val task1 = is_older((2002,3,29) , (2002,3,30));

(* task2 *)
fun number_in_month(date_list : (int*int*int) list, month : int) : int =
  let fun nm_r ([] : (int*int*int) list, acc : int, month : int) = acc
    | nm_r ((y, m, d) :: xs, acc, month) = if month = m then nm_r(xs, acc + 1, month) else nm_r(xs, acc, month)
  in
      nm_r(date_list, 0, month)
  end

val task2 = number_in_month([(2002,1,29) , (2002,1,30) , (2002,1,1)], 1);

(* task3 *)
fun number_in_month(date_list : (int*int*int) list, month_list : int list) : int =
    let fun date_r ([] : (int*int*int) list, acc : int, month : int) = acc
    | date_r ((y, m, d) :: xs, acc, month) = if month = m then date_r(xs, acc+1, month) else date_r(xs, acc, month)

    fun month_r (date_list, [], acc) = acc
      | month_r (date_list, x::xs, acc) = month_r(date_list, xs, date_r(date_list, acc, x))
    in
        month_r(date_list, month_list, 0)
    end

val task3 = number_in_month([(2002,1,29) , (2002,2,30) , (2002,3,1)], [1, 2, 5]);

(* task4 *)
fun dates_in_month(date_list : (int*int*int) list, month : int) : (int*int*int) list =
  let fun dm_r ([] : (int*int*int) list, acc :  (int*int*int) list, month : int) = acc
    | dm_r ((y, m, d) :: xs, acc, month) = if month = m then dm_r(xs, acc @ [(y,m,d)], month) else dm_r(xs, acc, month)
  in
      dm_r(date_list, [], month)
  end

val task4 = dates_in_month([(2002,3,29) , (2002,2,30) , (2002,3,1)], 3);

(* task5 *)
fun dates_in_month(date_list : (int*int*int) list, month_list : int list) : (int*int*int) list =
  let fun date_r ([] : (int*int*int) list, acc : (int*int*int) list, month : int) = acc
    | date_r ((y,m,d)::xs, acc, month) = if (month = m) then date_r(xs, acc @ [(y,m,d)], month) else date_r(xs, acc, month)

  fun month_r (date_list : (int*int*int) list, [] : int list, acc : (int*int*int) list) = acc
    | month_r (date_list, x::xs, acc) = month_r(date_list, xs, date_r(date_list, acc, x))
  in
      month_r(date_list, month_list, [])
  end

val task5 = dates_in_month([(2002,2,29) , (2002,2,30) , (2002,2,1)], [1, 2, 5]);

(* task6 *)
fun get_nth(string_list : string list, n : int) : string =
  let fun get_nth_r ([] : string list, n, acc) = "Index out of range"
    | get_nth_r (x::xs : string list, n, acc) = if (acc = n) then x else get_nth_r(xs, n, acc+1)
  in
      get_nth_r(string_list, n, 1)
  end

val task6 = get_nth(["a", "b", "c", "d"], 3);

(* task7 *)
fun date_to_string(date : (int*int*int)) : string = get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], (#2 date))^" "^Int.toString(#3 date)^","^Int.toString(#1 date)

val task7 = date_to_string((2002,3,29));

(* task8 *)
exception CorrectValueNotFound

fun number_before_reaching_sum(nums : int list, sum : int) : int =
  (* let fun list_sum ([] : int list) : int = 0
        | list_sum (x::xs) : int = x + list_sum(xs) *)

  let fun nbrs_r ([], n, bsum, NONE : int option) : int = raise CorrectValueNotFound
            | nbrs_r ([], n, bsum, result) = valOf result
            | nbrs_r (xn::xn1::nms, n, bsum, result) = if (((bsum + xn) < sum) andalso (bsum + xn + xn1 >= sum)) then nbrs_r(xn1::nms, n+1, bsum+xn, SOME(n)) else nbrs_r(xn1::nms, n+1, bsum+xn, result)
            | nbrs_r (nms, n, bsum, NONE) = raise CorrectValueNotFound
            | nbrs_r (nms, n, bsum, result) = valOf result
  in
      nbrs_r(nums, 1, 0, NONE)
  end

val task8 = number_before_reaching_sum([10, 20, 50, 1], 31)

(* task9 *)
fun what_month(day_of_year : int) : int = (number_before_reaching_sum([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], day_of_year : int) + 1) handle CorrectValueNotFound => 1

val task9 = what_month(300);

(* task10 *)
fun month_range(day1 : int, day2 : int) : int list =
    let fun mr_r (day : int) : int list = if day <= day2 then what_month(day)::mr_r(day+1) else []
    in
        mr_r(day1)
    end

val task10 = month_range(29, 35);

(* task11 *)
fun oldest(date_list : (int*int*int) list) : (int*int*int) option =
  let fun o_r ([] : (int*int*int) list, NONE : (int*int*int) option) : (int*int*int) option = NONE
    | o_r ([], od) = od
    | o_r (d::ds, NONE) = o_r(ds, SOME(d))
    | o_r (d::ds, od) = if is_older(valOf od, d) > 0 then o_r(ds, SOME(d)) else o_r(ds, od)
  in
      o_r(date_list, NONE)
  end

val task11 = oldest([(2002,2,29) , (2002,2,30) , (2001,2,1) , (2000,2,1) , (2002,1,1) , (1999,5,10) , (1999,5,9)]);
