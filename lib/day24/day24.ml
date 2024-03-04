open Core

let filename = "lib/day24/day24.input.txt"
let day24Input = In_channel.read_lines filename

let items = List.map day24Input ~f:(int_of_string)
let sum = List.fold ~init:0 ~f:(+) items

let wanted = sum / 3


(* Returns a list of all combinations that sums to n *)
let getWanted n lst =
  let rec inner sum used unused lst =
    match lst with 
    | [] -> if sum = n then [used] else []
    | hd :: tl -> if hd > n - sum  then
                    inner sum used (hd::unused) tl
                  else
                    inner (sum + hd) (hd::used) unused tl @ inner sum used (hd::unused) tl
  in
  inner 0 [] [] lst

let shortest lst = 
  List.fold ~init:100 ~f:(min) ( List.map ~f:(List.length) lst)

let wantedList = getWanted wanted items
let lengthOfShortestList = shortest wantedList

let shortestLists = List.filter wantedList ~f:(fun l -> List.length l = lengthOfShortestList)

let quantumEntanglement lst = List.fold ~init:1 ~f:( * ) lst

let qeList = List.map shortestLists ~f:(quantumEntanglement)
let resultP1 = List.hd_exn (List.sort qeList ~compare:(compare))

let wanted = sum / 4
let wantedList = getWanted wanted items
let lengthOfShortestList = shortest wantedList

let shortestLists = List.filter wantedList ~f:(fun l -> List.length l = lengthOfShortestList)
let qeList = List.map shortestLists ~f:(quantumEntanglement)

let resultP2 = List.hd_exn (List.sort qeList ~compare:(compare))

