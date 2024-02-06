open Core

let day11Input = "cqjxjnds"

let valOfChar c = int_of_char c - int_of_char 'a'
let charOfVal n = char_of_int (n + int_of_char 'a')
let incChar c  = if Char.equal c 'z' then ('a',1) else (charOfVal ( (valOfChar c) + 1 ),0)

let incrementWord s =
  let rec incrInner lst =
    match lst with 
    | [] -> ([],0)
    | [c] when Char.equal c 'z' -> (['a'] , 1)
    | [c] -> ([fst (incChar c)], 0)
    | h :: t -> let (lst, carry) = incrInner t in
                if carry > 0 then 
                  let (newChar, newCarry) = incChar h in ((newChar::lst), newCarry)
                else ((h::lst), 0)
  in 
  let result = incrInner (String.to_list s) in
  String.of_char_list  (fst result)
                    
let hasStraightThree s = 
  let lst = List.map ~f:(int_of_char) (String.to_list s) in
  let rec checkList lst = 
    match lst with 
    | [] -> false
    | _::[] -> false
    | _::_::[] -> false
    | a::b::c::[] -> (c = b + 1 ) && ( b = a + 1)
    | a::b::c::tl -> (c = b + 1 ) && ( b = a + 1) || checkList (b::c::tl)
  in checkList lst

let containsIOL s = String.exists ~f:(fun c -> List.exists ~f:(fun fb -> Char.equal fb c) ['i';'o';'l'] ) s

let rec countDoubles currDoubles count lst =
  match lst with
  | [] -> count
  | _ :: [] -> count
  | a :: b :: tl -> if (Char.equal a b) && not (List.exists currDoubles ~f:(fun c -> Char.equal a c))
      then countDoubles (a::currDoubles) (count+1) (b::tl)
      else countDoubles currDoubles count (b::tl)

let hasTwoDoubles s =
  let lst = String.to_list s in
  let noOfDoubles  = countDoubles [] 0 lst in
    noOfDoubles >= 2

let isValid s = 
  hasStraightThree s && not (containsIOL s) && hasTwoDoubles s

let rec nextValidPassword s = 
  let next = incrementWord s in
  if isValid next then next
  else nextValidPassword next

let resultP1 = nextValidPassword day11Input

let resultP2 = nextValidPassword resultP1