open Core

let filename = "lib/day1/day1.input.txt"
let day1Input = In_channel.read_all filename

let isLeft c = Char.equal c '('

let lefts = String.count ~f:(isLeft) day1Input
let rights = (String.length day1Input ) - lefts

let resultP1 = lefts - rights

let findFirstBasementPos day1Input =
  let rec inner idx floor  = 
    match floor with 
    | -1 -> idx
    | fl -> if isLeft day1Input.[idx] 
            then inner (idx + 1) (fl + 1)
            else inner (idx + 1) (fl - 1)
in inner 0 0

let resultP2 = findFirstBasementPos day1Input