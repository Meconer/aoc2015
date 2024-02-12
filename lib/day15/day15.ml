open Core
(* let filename = "lib/day15/day15.example.txt" *)
let filename = "lib/day15/day15.input.txt"

let day15Input = In_channel.read_lines filename

type properties = {
  name : string;
  capacity : int;
  durability : int;
  flavor : int;
  texture : int;
  calories : int;
}

let remove_last (s: string) : string =
  if String.is_empty s then
    s
  else
    String.sub s ~pos:0 ~len:(String.length s - 1)

let processInput (lines : string list) : properties list =
  let rec inner acc lines =
  match lines with 
  | [] -> List.rev acc
  | line :: tl ->  let parts = String.split ~on:' ' line in
      let name = remove_last (List.hd_exn parts) in
      let capacity = int_of_string (remove_last (List.nth_exn parts 2) )in
      let durability = int_of_string (remove_last (List.nth_exn parts 4) )in
      let flavor = int_of_string (remove_last (List.nth_exn parts 6) )in
      let texture = int_of_string (remove_last (List.nth_exn parts 8) )in
      let calories = int_of_string (List.last_exn parts )in
      inner ({name = name;
        capacity = capacity;
        durability = durability;
        flavor = flavor;
        texture = texture;
        calories = calories
      }::acc)  tl

  in inner [] lines

let ingredients = processInput day15Input


let rec distribute_all total_items num_containers =
  if num_containers = 1 then
    [[total_items]]
  else
    List.concat_map ~f:(fun first_container ->
      let remaining = total_items - first_container in
      List.map ~f:(fun dist ->
        first_container :: dist
      ) (distribute_all remaining (num_containers - 1))
    ) (List.init (total_items + 1) ~f:Fn.id)

let distributions = distribute_all 100 (List.length ingredients)

let capacity distribution ingredients = 
  max 0 (
  List.fold2_exn ~init:0 ~f:(fun sum dist ing -> sum + dist * ing.capacity) distribution ingredients)

let durability distribution ingredients = 
  max 0 (
  List.fold2_exn ~init:0 ~f:(fun sum dist ing -> sum + dist * ing.durability) distribution ingredients)

let flavor distribution ingredients = 
  max 0 (
  List.fold2_exn ~init:0 ~f:(fun sum dist ing -> sum + dist * ing.flavor) distribution ingredients)

let texture distribution ingredients = 
  max 0 (
  List.fold2_exn ~init:0 ~f:(fun sum dist ing -> sum + dist * ing.texture) distribution ingredients)

let calories distribution ingredients = 
  max 0 (
  List.fold2_exn ~init:0 ~f:(fun sum dist ing -> sum + dist * ing.calories) distribution ingredients)

let score distribution ingredients = 
  capacity distribution ingredients * durability distribution ingredients *
  flavor distribution ingredients * texture distribution ingredients

let scores = List.map ~f:(fun d -> score d ingredients) distributions

let resultP1 = List.fold ~init:0 ~f:(max) scores

let distP2 = List.filter ~f:(fun d -> (calories d ingredients) = 500 ) distributions
let scoresP2 = List.map ~f:(fun d -> score d ingredients) distP2
let resultP2 = List.fold ~init:0 ~f:(max) scoresP2