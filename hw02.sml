(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s : string, lst : string list) =
     let fun aeo_r (bef : string list, []) : string list option = NONE
            | aeo_r (bef, xs::aft) = if(same_string(xs,s)) then SOME(bef@aft) else aeo_r(bef@[xs], aft)
     in
        aeo_r([], lst)
     end


val task1 = all_except_option("2", ["4","3","2","1"]);

fun get_substitutions1 ([] : string list list, s : string) : string list = []
    | get_substitutions1 (xs::replace, s) =
    let fun aeo_r (bef : string list, []) = []
           | aeo_r (bef, xs::aft) = if(same_string(xs,s)) then bef@aft else aeo_r(bef@[xs], aft)
    in
       aeo_r([], xs)@get_substitutions1(replace, s)
    end

val task2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")


fun get_substitutions2(replace : string list list, s : string) : string list =
    let fun aeo_r (bef : string list, []) = []
           | aeo_r (bef, xs::aft) = if(same_string(xs,s)) then bef@aft else aeo_r(bef@[xs], aft)
        fun aeo_r1 ([], result) = result
           | aeo_r1 (xs::replace, result) = aeo_r1(replace, result@aeo_r([],xs))
    in
       aeo_r1(replace, [])
    end

val task3 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

fun similar_names ([] : string list list, name : {first:string, middle:string, last:string}) : {first:string,middle:string,last:string} list = [name]
    | similar_names (xs::replace, {first, middle, last}) =
    let fun aeo_r (bef, [], return : bool) = if return then bef else []
           | aeo_r (bef, xs::aft, return) = if(same_string(xs, first)) then aeo_r(bef, aft, true) else aeo_r(bef@[{first=xs, middle=middle, last=last}], aft, return)
    in
       similar_names(replace, {first=first, middle=middle, last=last})@aeo_r([], xs, false)
    end

val task4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})



(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
