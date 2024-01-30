open Core

let filename = "lib/day3/input.txt"
let day1Input = In_channel.read_all filename

type pos = {x : int; y : int}

let posToKey pos = (string_of_int pos.x) ^ ":" ^ (string_of_int pos.y)

type t = (string, int, String.comparator_witness) Map.t

let houseMap = Map.empty (module String)
let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.set t ~key:s ~data:(count + 1)

let move c p = 
  match c with 
  | '^' -> {x = p.x ; y = p.y + 1}
  | 'v' -> {x = p.x ; y = p.y - 1}
  | '<' -> {x = p.x - 1 ; y = p.y}
  | '>' -> {x = p.x + 1 ; y = p.y}
  | _ -> failwith "Wrong char in input"

let parseInput s = 
  let limit = String.length s in
  let rec inner acc i p = 
    let _ = Printf.printf "%d %d\n" p.x p.y in
    if  i = limit  then acc
    else let newPos = move s.[i] p in
      inner (touch acc (posToKey p)) (i + 1) newPos
  in inner houseMap 0 { x = 0; y = 0 }

let resultMap = parseInput day1Input

let resultP1 = List.length (Map.to_alist resultMap)