open Core

let filename = "lib/day2/input.txt"
let day1Input = In_channel.read_lines filename


let areaFromLine s = 
  let dims = List.map ~f:int_of_string (String.split ~on:'x' s) in
  let sortedDims = List.sort ~compare:compare dims in
  match  sortedDims with
  | a::b::c::_ ->   3 * a * b + 2 * b * c + 2 * a * c
  | _ -> failwith "Must be three numbers in list"
  
let resultP1 = List.fold ~init:0 ~f:(+) (List.map ~f:areaFromLine day1Input)

let ribbonLengthFromLine s = 
  let dims = List.map ~f:int_of_string (String.split ~on:'x' s) in
  let sortedDims = List.sort ~compare:compare dims in
  match  sortedDims with
  | a::b::c::_ ->   2 * a + 2 * b  + a * b * c
  | _ -> failwith "Must be three numbers in list"

let resultP2 = List.fold ~init:0 ~f:(+) (List.map ~f:ribbonLengthFromLine day1Input)
