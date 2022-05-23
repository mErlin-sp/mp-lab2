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

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color((suit, rank) : card) : color =
     case suit of
        Clubs => Black
      |  Diamonds => Red
      |  Hearts => Red
      |  Spades => Black

val task2a = card_color((Spades, Queen));

fun card_value((suit, rank) : card) : int =
     case rank of
        Jack => 10
      |  Queen => 10
      |  King => 10
      |  Ace => 11
      |  Num i => i

val task2b = card_value((Diamonds, King));

fun remove_card(cs : card list, c : card, e : exn) : card list =
    let fun rc_r (bef : card list, [] : card list) = raise e
           | rc_r (bef, cc::aft) = if(cc = c) then bef@aft else rc_r(bef@[cc], aft)
    in
       rc_r([], cs)
    end

val task2c = remove_card([(Spades, Num 6),(Hearts, Num 9),(Diamonds, Ace),(Clubs, Queen),(Spades, King)] : card list, (Hearts, Num 9) : card, IllegalMove);

fun all_same_color([] : card list) : bool = true
    | all_same_color (c::cc::cs) = if card_color c <> card_color cc then false else all_same_color(cc::cs)
    | all_same_color (c) = true

val task2d = all_same_color([(Diamonds, Num 6),(Diamonds, Num 9),(Diamonds, Ace),(Hearts, Queen),(Diamonds, King)] : card list);

fun sum_cards(cs : card list) : int =
    let fun sc_r ([] : card list, sum : int) : int = sum
           | sc_r (c::cs, sum : int) = sc_r(cs, sum + card_value c)
    in
       sc_r(cs, 0)
    end

val task2e = sum_cards([(Diamonds, Num 6),(Diamonds, Num 9),(Diamonds, Ace),(Hearts, Queen),(Diamonds, King)] : card list);

fun score(cs : card list, goal : int) : int =
    let
        val sum = sum_cards cs
        val prev_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color cs = false then prev_score else prev_score div 2
    end


val task2f = score([(Diamonds, Num 6),(Diamonds, Num 9),(Diamonds, Ace),(Hearts, Queen),(Diamonds, King)] : card list, 100);

fun officiate(cs : card list, ms : move list, goal : int) : int =
    let fun of_r ([] : move list, hold : card list, cs : card list, sc : int) : int = sc
           | of_r (Draw :: ms, hold, [], sc) : int = sc
           | of_r (Draw :: ms, hold, c :: cs, sc) : int = if sc > goal then sc else of_r(ms, hold@[c], cs, score(hold@[c], goal))
           | of_r (Discard mv :: ms, hold, cs, sc) : int = if sc > goal then sc else
              let
                  val rh = remove_card(hold, mv : card, IllegalMove)
              in
                  of_r(ms, rh, cs, score(rh, goal))
              end
    in
       of_r(ms, [], cs, 0)
    end

val game = officiate([(Diamonds, Num 6),(Diamonds, Num 9),(Diamonds, Ace),(Hearts, Queen),(Diamonds, King)] : card list, [Draw, Draw, Draw, Draw, Discard (Diamonds, Num 6)] : move list, 100);
