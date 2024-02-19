open Core
let filename = "lib/day19/day19.input.txt"
(* let filename = "lib/day19/day19.example.txt" *)

let day19Input = In_channel.read_lines filename

let rec processInput acc  = function
  | [] -> List.rev acc
  | h :: t -> processInput ((int_of_string h)::acc) t

let split_on_empty_line lst =
  let rec aux acc current = function
    | [] -> List.rev (current :: acc)
    | "" :: tl -> aux (current :: acc) [] tl
    | hd :: tl -> aux acc (hd :: current) tl
  in
  aux [] [] lst


let formula = 
  let parts = split_on_empty_line day19Input in
   List.hd_exn (List.last_exn parts)


let changes = 
  let getChangeFromLine line = 
    let parts = String.split ~on:' ' line in
    (List.hd_exn parts, List.last_exn parts) in

  let parts = split_on_empty_line day19Input in 

  List.map (List.hd_exn parts) ~f:getChangeFromLine


(*
 Can also be done with the oneliner
 Map.of_alist_multi (module String) changes

 *)
 (* let replacements changes = Map.of_alist_multi (module String ) changes *)

let replacements changes = 
  let rec inner mapAcc changes =
    match changes with
    | [] -> mapAcc
    | hd :: tl -> match Map.find mapAcc (fst hd) with 
      | None -> inner (Map.set mapAcc ~key:(fst hd) ~data:([snd hd])) tl
      | Some r -> inner (Map.set mapAcc ~key:(fst hd) ~data:((snd hd)::r)) tl
  in
  inner (Map.empty (module String)) changes

let changeMap = replacements changes

let buildSet prev s rest = 
      let replacements = 
        match Map.find changeMap s with
        | None -> [s]
        | Some l -> l 
      in 
          let newFormulas = List.map replacements ~f:(fun repl ->
          (String.of_char_list prev) ^ repl ^ (String.of_char_list rest)
          ) in
        Set.of_list (module String) newFormulas

let makeVariants formula = 
  let rec inner acc prev rest = 
    match rest with 
    | [] -> acc
    | c1::c2::tl when Char.is_uppercase c1 && Char.is_lowercase c2 -> 
      let newSet = buildSet prev (String.of_char_list [c1; c2]) tl in
        inner  (Set.union acc newSet ) (prev @ (c1::[c2])) tl

    | c1::c2::tl when Char.is_uppercase c2 ->
        if Char.is_lowercase c1 then failwith "Should be uppercase" else
        let newSet = buildSet prev (String.of_char c1) (c2::tl) in
        inner  (Set.union acc newSet) (prev @ [c1]) (c2::tl)

    | c1::[] -> 
      Printf.printf "%c\n" c1;

      if Char.is_uppercase c1 then 
        let newSet = buildSet prev (String.of_char c1) [] in
        inner (Set.union acc newSet) (prev @ [c1]) []
      else failwith "Single lc at end"
    | _::_ -> failwith "Unexpected"

  in inner (Set.empty (module String)) [] (String.to_list formula)

(* Remove the original formula. I don't understand how it gets there*)
let resultSet = Set.remove (makeVariants formula) formula

let resultP1 = Set.length resultSet

let lSortedChanges = List.sort changes ~compare:(fun a b -> compare (String.length (snd b)) (String.length (snd a)))

let doReplace formula changeList = 
  let rec inner count s lst =
  if String.length s = 0 then failwith "No string left"
  else 
    match lst  with 
    | [] -> count
    | hd::tl -> 
      if String.is_substring s ~substring:(snd hd) then
        let s' = String.substr_replace_first ~pattern:(snd hd) ~with_:(fst hd ) s in
      inner  (count + 1) s' changeList
      else inner count s tl

    in inner 0 formula changeList



let resultP2 = doReplace formula lSortedChanges