open Core 


let filename = "lib/day12/day12.input.txt"
let day12Input = In_channel.read_all filename

let numOf c = int_of_char c - int_of_char '0'

let parseNumbers s =
 
  let rec inner acc inDigit negFact currNum lst =
    match lst with 
    | [] -> if inDigit then (negFact * currNum) :: acc else acc
    | c :: t when Char.equal c '-' -> inner acc true (-1) 0 t
    | c :: t when Char.is_digit c -> inner acc true negFact (currNum * 10 + numOf c) t
    | _ :: t -> if inDigit then
                  let number = negFact *  currNum in
                  inner (number::acc) false 1 0 t
                else
                  inner acc false 1 0 t
  in 
  let lst = String.to_list s in inner [] false 1 0 lst


let numbers = parseNumbers day12Input
(* let resultP1 = List.fold ~init:0 ~f:(+) numbers *)


type token_type =
  | String of string
  | Number of int
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Colon
  | Comma

type token = { token_type: token_type; value: string }

let tokenize (input: string) : token list =
  let rec tokenize_helper (input: char list) (acc: token list) : token list =
    match input with
    | [] -> List.rev acc
    | hd :: tl ->
      match hd with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_helper tl acc (* Ignore whitespace *)
      | '{' -> tokenize_helper tl ({ token_type = LeftBrace ; value = "{" } :: acc)
      | '}' -> tokenize_helper tl ({ token_type = RightBrace; value = "}" } :: acc)
      | '[' -> tokenize_helper tl ({ token_type = LeftBracket; value = "[" } :: acc)
      | ']' -> tokenize_helper tl ({ token_type = RightBracket; value = "]" } :: acc)
      | ':' -> tokenize_helper tl ({ token_type = Colon; value = ":" } :: acc)
      | ',' -> tokenize_helper tl ({ token_type = Comma; value = "," } :: acc)
      | '"' -> tokenize_string tl acc
      | c -> tokenize_keyword (c :: tl) acc
  and tokenize_string (input: char list) (acc: token list) : token list =
    let rec read_string (input: char list) (acc: char list) : char list * char list =
      match input with
      | '"' :: tl ->  (tl, List.rev acc)
      | '\\' :: '\\' :: tl -> read_string tl ('\\' :: acc)  (* Escaped backslash *)
      | '\\' :: '"' :: tl -> read_string tl ('"' :: acc)    (* Escaped double quote *)
      | hd :: tl -> read_string tl (hd :: acc)
      | [] -> failwith "Unterminated string"
    in
    let (rest_input, value) = read_string input [] in
    tokenize_helper rest_input ({ token_type = String (String.of_char_list value); value = (String.of_char_list value) } :: acc)
  and tokenize_keyword (input: char list) (acc: token list) : token list =
    let rec read_keyword (input: char list) (acc: char list) : char list * char list =
      match input with
      | [] -> (input, List.rev acc)
      | hd :: tl when Char.equal hd ' ' || Char.equal hd '\t' || Char.equal hd '\n' || Char.equal hd  '\r' -> (tl, List.rev acc)
      | hd :: tl when Char.is_digit hd || Char.equal hd '-' -> read_keyword tl (hd :: acc)
      | _ :: _ -> (input, List.rev acc)
      
    in
    let (rest_input, value) = read_keyword input [] in
      (* List.iter ~f:(fun token ->Printf.printf "%s\n" token.value) (List.rev acc) ; *)
      (* Printf.printf "%s\n" (String.of_char_list value); *)
      let num_value = int_of_string (String.of_char_list value) in
      tokenize_helper rest_input ({ token_type = Number num_value; value = String.of_char_list value } :: acc)
  in
  tokenize_helper (String.to_list input) []



type json_value =
  | String of string
  | Number of int
  | Struct of (string * json_value) list
  | Array of json_value list


let parse_json (tokens : token list) : json_value =
  let rec parse_value (tokens : token list) : json_value * token list =
    match tokens with 
    | [] -> failwith "Unexpected end of tokens"
    | {token_type = String s; _} :: tl -> (String s, tl)
    | {token_type = Number n; _} :: tl -> (Number n, tl)
    | {token_type = LeftBrace; _} :: tl -> parse_struct tl
    | {token_type = LeftBracket; _} :: tl -> parse_array tl
    | {token_type = token_type; value = value} :: _ -> failwith ("Unexpected token in parse_value" ^ value ^ " (" ^ (string_of_token_type token_type) ^ ")")


  and parse_struct (tokens : token list) : json_value * token list =

    let rec parse_members (tokens : token list) (acc : (string * json_value) list) : (string * json_value) list * token list =
      match tokens with 
      | [] -> failwith "Unexpected end of tokens in parse_members"
      | {token_type = RightBrace; _ } :: tl -> (List.rev acc, tl)
      | {token_type = Comma; _ } :: tl -> parse_members tl acc
      | _ -> let (name, tokens') = parse_value tokens in
        match tokens' with 
        | [] -> failwith "Unexpected end of tokens in parse_members"
        | {token_type = Colon; _ }::tl -> let (value, tokens'') = parse_value tl in (parse_members tokens'' ((get_name_from_json_value name, value) :: acc))
        | {token_type = tt; value = v}::_ -> failwith ("Unexpected token : " ^ (string_of_token_type tt) ^ " : " ^ v)
    in
    let (members, rest_tokens ) = parse_members tokens [] in (Struct members, rest_tokens)

    
  and get_name_from_json_value (j : json_value) : string =
    match j with 
    | String s -> s
    | _ -> failwith "Must be a string"


  and parse_array (tokens : token list) : json_value * token list =
    let rec parse_elements (tokens: token list) (acc: json_value list ) : json_value list * token list = 
      match tokens with 
      | [] -> failwith "Unexpected end of tokens in parse_elements"
      | {token_type = RightBracket; _ } :: tl -> (List.rev acc, tl)
      | {token_type = Comma; _ } :: tl -> parse_elements tl acc
      | _ -> let (value, tokens') = parse_value tokens in
          parse_elements tokens' (value::acc)
    in
    let (elements, rest_tokens) = parse_elements tokens [] in
      (Array  elements, rest_tokens)

  and string_of_token_type (token_type: token_type) : string =
    match token_type with
    | String _ -> "String"
    | Number _ -> "Number"
    | LeftBrace -> "LeftBrace"
    | RightBrace -> "RightBrace"
    | LeftBracket -> "LeftBracket"
    | RightBracket -> "RightBracket"
    | Colon -> "Colon"
    | Comma -> "Comma"
  in
  let (value, rest_tokens) = parse_value tokens in
  match rest_tokens with 
  | [] -> value
  | _ -> failwith "Unexpected tokens after parsing"
  

let rec sum_numbers (json : json_value ) : int =
  match json with
  | String _ -> 0
  | Number n -> n
  | Struct s -> List.fold ~f:(+) ~init:0 (List.map ~f:(fun (_,v) -> sum_numbers v) s)
  | Array a -> List.fold ~f:(+) ~init:0 (List.map ~f:( sum_numbers ) a)

let get_json_string (json: json_value)  = 
  match json with 
  | String s -> s
  | _ -> ""


let rec sum_numbers_without_red (json: json_value) : int =
  match json with
  | String _ -> 0
  | Number n -> n
  | Struct s ->  
    let hasRed = (List.exists ~f:(fun (_,v) -> String.equal (get_json_string v) "red") s) in
        if hasRed then 0 else List.fold ~f:(+) ~init:0 (List.map ~f:(fun (_,v) -> sum_numbers_without_red v) s) 
  | Array a -> List.fold ~f:(+) ~init:0 (List.map ~f:( sum_numbers_without_red ) a)

let tokens = tokenize day12Input
let json = parse_json tokens

let resultP1 = sum_numbers json
let resultP2 = sum_numbers_without_red json
