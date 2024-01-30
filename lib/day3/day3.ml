open Core

let filename = "lib/day3/input.txt"
let day1Input = In_channel.read_all filename

type pos = {x : int; y : int}

let posToKey pos = (string_of_int pos.x) ^ ":" ^ (string_of_int pos.y)

type t = (string, int, String.comparator_witness) Map.t

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
    (* let _ = Printf.printf "%d %d\n" p.x p.y in *)
    if  i = limit  then acc
    else let newPos = move s.[i] p in
      inner (touch acc (posToKey p)) (i + 1) newPos
  in 
  let houseMap = Map.empty (module String) in
  inner houseMap 0 { x = 0; y = 0 }

let resultMap = parseInput day1Input

let resultP1 = List.length (to_list resultMap)


let parseInputP2 s = 
  let limit = String.length s in
  if limit mod 2 > 0 then failwith "Uneven input" ;
  let rec inner acc i roboPos santaPos = 
    (* let _ = Printf.printf "%d %d\n" p.x p.y in *)
    if  i = limit  then acc
    else let newSantaPos = move s.[i] santaPos in
      let newRoboPos = move s.[i+1] roboPos in
      let newAcc = touch (touch acc (posToKey newRoboPos)) (posToKey newSantaPos) in
      
      inner newAcc (i + 2) newRoboPos newSantaPos in

  let houseMap = Map.empty (module String) in
  let houseMap' = touch houseMap (posToKey {x=0;y=0}) in
  inner houseMap' 0 { x = 0; y = 0 } {x = 0; y = 0}


let resultMap = parseInputP2 day1Input

let resultP2 = List.length (to_list resultMap)



