open Core
(* let filename = "lib/day13/day13.example.txt" *)
let filename = "lib/day13/day13.input.txt"

let day13Input = In_channel.read_lines filename

type edge = {
  person1 : string;
  person2 : string;
  cost : int
}

let processInput (lines : string list)=
  let rec inner accPeople accEdge lines =
  match lines with 
  | [] -> let people = Set.to_list (Set.of_list (module String) accPeople) in
              (people, accEdge)
  | line :: tl ->  let parts = String.split ~on:' ' line in
      let person1 = List.hd_exn parts in
      let person2 = String.rstrip ~drop:(Char.equal '.') (List.last_exn parts) in
      let happiness = let factor = if String.equal (List.nth_exn parts 2) "gain" then 1 else -1 in
        factor * (int_of_string (List.nth_exn parts 3)) in

      (* Printf.printf "%s %s %d\n" person1 person2 happiness; *)
      inner (person1::person2::accPeople) ({person1 = person1; person2 = person2; cost = happiness}::accEdge) tl

  in inner [] [] lines

let (vertices, edges) = processInput day13Input

let rec split_at pos lst =
  match pos, lst with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | _, x :: xs ->
    let (ys, zs) = split_at (pos - 1) xs in
    (x :: ys, zs)

let rec permutations lst = 
  match lst with 
  | [] -> [[]]
  | x :: xs ->
    let perms = permutations xs in
    List.concat_map ~f:(fun perm ->
      List.init  (List.length perm + 1) ~f:(fun pos ->
        let (ys, zs) = split_at pos perm in
        ys @ [x] @ zs
      )
    ) perms

let allOrders lst = 
  match lst with 
  | [] -> failwith "At least 1 element in list"
  | h::t ->  let perms = permutations t in
      List.map ~f:(fun perm -> h::perm) perms

let myName = "Me"

let getHappiness person1 person2  = 
  let rec inner lst = 
    match lst with 
    | [] -> failwith "Persons not found"
    | h :: t -> if (String.equal h.person1 person1) && (String.equal h.person2 person2 ) then
                  (* let _ = Printf.printf "%s : %s - %d\n" person1 person2 h.cost in *)
                  h.cost
                else inner t
  in
  if String.equal person1 myName || String.equal person2 myName then 0
  else inner edges

let calcHappiness lst = 
  let rec inner acc lst =
    match lst with 
    | [] -> failwith "This should not happen" 
    | [_] -> acc
    | a::b::t -> inner (acc + (getHappiness a b)
        + (getHappiness b a)) (b::t)
  in 
  getHappiness (List.hd_exn lst) (List.last_exn lst) +
  getHappiness (List.last_exn lst) (List.hd_exn lst) +
  inner 0 lst

let perms = allOrders vertices
let happinesses = List.map  ~f:(calcHappiness) perms

let resultP1 = List.fold ~init:0 ~f:max happinesses

let p2Vertices = myName :: vertices

let permsP2 = allOrders p2Vertices
let happinessesP2 = List.map  ~f:(calcHappiness) permsP2
let resultP2 = List.fold ~init:0 ~f:max happinessesP2