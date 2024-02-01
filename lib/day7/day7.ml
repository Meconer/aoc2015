open Core
let filename = "lib/day7/day7.input.txt"
let day7Input = In_channel.read_lines filename

let isNumber  s = not (String.exists s ~f:(fun c -> not (Char.is_digit c)))

let getValueOf constOrVar circuit = 
  if isNumber constOrVar then Some (int_of_string constOrVar)
  else Map.find circuit constOrVar 

let doOperation op v1 v2 = 
  match v1, v2 with 
  | None, _ -> None
  | _, None -> None
  | Some v1p, Some v2p ->
    match op with 
    | "OR" ->  Some (v1p lor v2p)
    | "AND" ->  Some (v1p land v2p)
    | "LSHIFT" -> Some (v1p lsl v2p)
    | "RSHIFT" ->  Some (v1p lsr v2p)
    | _ -> failwith "Illegal operator"

let doExpr s circuit = 
  let parts = String.split ~on:' ' s in
  if List.length parts = 1 then getValueOf (List.hd_exn parts) circuit
  else 
      if List.length parts = 2 then 
        if not (String.equal (List.hd_exn parts) "NOT") then failwith "Must be NOT here" 
        else
          let v = getValueOf (List.last_exn parts) circuit in
          match v with 
          | None -> None
          | Some v -> Some (lnot v)
      else (* length must be 3*)
      let v1 = getValueOf (List.hd_exn parts) circuit in
      let v2 = getValueOf (List.last_exn parts) circuit in
      doOperation (List.nth_exn parts 1 ) v1 v2

let processLine line circuit = 
  let wireName = String.strip (String.rtake_while line ~f:(fun c -> not (Char.equal c '>'))) in
  let value = String.strip (String.take_while line ~f:(fun c -> not (Char.equal c '-'))) in
  let result = doExpr value circuit in
  match  result with
  | None -> circuit
  | Some r -> Map.set circuit ~key:wireName ~data:r


let rec processLines lines circuit =
  match lines with 
  | [] -> circuit
  | h :: t -> processLines t (processLine h circuit)

let calcA lines = 
  let circuit = Map.empty (module String) in

  let rec iterate circuit lines =
    let circuit' = processLines lines circuit in
    let a = Map.find  circuit' "a" in
    if is_none a then
      iterate circuit' lines
    else match a with
    | None -> failwith "Wtf??"
    | Some a -> a
  
  in iterate circuit lines

let resultP1 = calcA day7Input

let rec replaceB acc lines =
  match lines with 
  | [] -> List.rev acc
  | h :: t -> if String.is_suffix h ~suffix:"-> b" then
                replaceB (((string_of_int resultP1) ^ " -> b") :: acc) t
              else
                replaceB (h::acc) t

let resultP2 = calcA (replaceB [] day7Input)