open Core
let myfilename = "lib/day8/day8.input.txt"

let channel = In_channel.create myfilename

(* Reads all characters from the file. Add a \n char after each line so we
   can split them later*)
let char_list = List.rev (In_channel.fold_lines
    channel
    ~init:[]
    ~f:(fun acc line -> List.rev_append (String.to_list line @ ['\n']) acc)
)

(* Takes a list of characters and splits it into a list of lists of characters at the \n char*)
let rec buildLinesFromCharlist currLine acc cl =
  match cl with 
  | [] -> List.rev acc
  | h :: t -> if Char.equal h '\n' then buildLinesFromCharlist [] ((List.rev currLine)::acc) t 
              else buildLinesFromCharlist (h::currLine) acc t
              
(* Build the list of list of characters by line*)
let lineCharList = buildLinesFromCharlist [] [] char_list

(* Map the length to a number of characters of code for each line*)
let codeLengths = List.map  lineCharList ~f:(List.length)


(*Remove first and last characters of each lines which should be two "" -characters *)
let withFirstAndLastRemoved = List.map ~f:(fun s -> List.sub s ~pos:1 ~len:(List.length s - 2)) lineCharList

let hexCharToDigit c = 
  match c with 
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | c -> int_of_char c - int_of_char '0'

let charToDigit c = int_of_char c - int_of_char '0'

let rec removeEscapedChars acc lineCharList =
  match lineCharList with 
  | [] -> List.rev acc
  | '\\' :: 'x' :: n1 :: n2 :: t -> removeEscapedChars ((char_of_int ((hexCharToDigit n1)*16+(hexCharToDigit n2)))::acc) t
  | '\\' :: 'x' :: [_] -> failwith "\\x must be followed by two hex digits"
  | '\\' :: c :: t -> removeEscapedChars (c::acc) t
  | '\\' :: [] -> failwith "\\ must be followed by at least 1 char"
  | c :: t -> removeEscapedChars (c::acc) t
  
let withEscapesRemoved = List.map withFirstAndLastRemoved ~f:(fun lcl -> removeEscapedChars [] lcl)

let strLengths = List.map withEscapesRemoved ~f:(List.length ) 

let totalCodeChars = List.fold ~f:(+) ~init:0 codeLengths
let totalChars = List.fold ~f:(+) ~init:0 strLengths
let resultP1 = totalCodeChars - totalChars


(* Part 2 starts here *)

let rec escapeChars acc lineCharList =
  match lineCharList with 
  | [] ->  List.rev acc
  | '"' :: t -> escapeChars (List.rev_append ['\\'; '"']  acc) t
  | '\\' :: t -> escapeChars (List.rev_append ['\\';'\\']  acc) t
  | c :: t -> escapeChars (c::acc) t

let escaped = List.map lineCharList ~f:(fun lst -> escapeChars [] lst)
let escapedWithStartAndEndAdded = List.map escaped ~f:(fun lst -> '"' :: lst @ ['"'] )

let escapedLengths = List.map ~f:(List.length) escapedWithStartAndEndAdded

let totalCharsEscaped = List.fold ~init:0 ~f:(+) escapedLengths

let resultP2 = totalCharsEscaped - totalCodeChars