open Core

let day10Input = "1113222113"


let rec seeAndSay = function
  | [], nys -> List.rev nys
  | x::xs, [] -> seeAndSay(xs, [x; 1])
  | x::xs, y::n::nys when x=y -> seeAndSay(xs, y::1+n::nys)
  | x::xs, nys -> seeAndSay(xs, x::1::nys)

let charToInt c= int_of_char c - int_of_char '0'

let doSeeAndSay n s = 
  let lst = List.map ~f:(charToInt) (String.to_list s) in
  let rec inner n lst =
    if n = 0 then List.length lst
    else 
      let nextList = seeAndSay (lst, []) in
      inner (n-1) nextList
    in

    inner n lst

let resultP1 = doSeeAndSay 40 day10Input
let resultP2 = doSeeAndSay 50 day10Input
