open Core
let filename = "lib/day16/day16.input.txt"

let day16Input = In_channel.read_lines filename

type properties = | Number | Children | Cats | Samoyeds | Pomeranians | 
  Akitas | Vizslas | Goldfish | Trees | Cars | Perfumes

type sue  = {
  number : int;
  props : (properties * int ) list
}

let getVal propval = 
  int_of_string (String.rstrip ~drop:(fun c -> Char.equal ',' c || Char.equal c ':') propval)

let getPropertiesFromLine line = 
  let parts = String.split ~on:' ' line in 
  let rec inner acc lst =
    match lst with 
    | [] -> acc
    | prop::v::tl when String.equal prop "children:" -> inner (( Children , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "cats:" -> inner (( Cats , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "samoyeds:" -> inner (( Samoyeds , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "pomeranians:" -> inner (( Pomeranians , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "akitas:" -> inner (( Akitas , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "vizslas:" -> inner (( Vizslas , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "goldfish:" -> inner (( Goldfish , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "trees:" -> inner (( Trees , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "cars:" -> inner (( Cars , (getVal v))::acc) tl
    | prop::v::tl when String.equal prop "perfumes:" -> inner (( Perfumes , (getVal v))::acc) tl
    | _::tl -> inner acc tl
  in inner [] parts
    
let processInput lines =
  let rec inner acc lines =
    match lines with 
    | [] -> List.rev acc
    | line :: tl ->  
        let parts = String.split ~on:' ' line in
        let number = getVal (List.nth_exn parts 1) in
        let posOfFirstColon = String.index_exn line ':' in
        let propPart = String.sub ~pos:( posOfFirstColon) ~len:((String.length line ) - posOfFirstColon ) line in
        let props = (Number, number) :: getPropertiesFromLine propPart in 
          inner (props::acc) tl
  in inner [] lines
    

let propIsCorrect prop =
  match prop with 
  | (Children, n) -> n = 3
  | (Cats, n) -> n = 7
  | (Samoyeds, n) -> n = 2
  | (Pomeranians, n) -> n = 3
  | (Akitas, n) -> n = 0
  | (Vizslas, n) -> n = 0
  | (Goldfish, n) -> n = 5
  | (Trees, n) -> n = 3
  | (Cars, n) -> n = 2
  | (Perfumes, n) -> n = 1
  | (Number, _) -> true

let propIsCorrectP2 prop =
  match prop with 
  | (Children, n) -> n = 3
  | (Cats, n) -> n > 7
  | (Samoyeds, n) -> n = 2
  | (Pomeranians, n) -> n < 3
  | (Akitas, n) -> n = 0
  | (Vizslas, n) -> n = 0
  | (Goldfish, n) -> n < 5
  | (Trees, n) -> n > 3
  | (Cars, n) -> n = 2
  | (Perfumes, n) -> n = 1
  | (Number, _) -> true

let hasCorrectMemories sue = 
  not (List.exists ~f:(fun p -> not (propIsCorrect p)) sue)

let hasCorrectMemoriesP2 sue = 
  not (List.exists ~f:(fun p -> not (propIsCorrectP2 p)) sue)

let sues = processInput day16Input


let correctSue = List.filter ~f:(hasCorrectMemories) sues
let correctSueP2 = List.filter ~f:(hasCorrectMemoriesP2) sues
let resultP1 = snd (List.hd_exn (List.hd_exn correctSue))

let resultP2 = snd (List.hd_exn (List.hd_exn correctSueP2))
