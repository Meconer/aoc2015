open Core

let filename = "lib/day23/day23.input.txt"
let day23Input = In_channel.read_lines filename

type instruction = Hlf | Tpl | Inc | Jmp | Jie | Jio

type register = A | B

type programLine = {
  instr : instruction;
  reg : register;
  offset : int
}

let instr_of_string s = 
  match s with 
  | "hlf" -> Hlf
  | "tpl" -> Tpl
  | "inc" -> Inc
  | "jmp" -> Jmp
  | "jie" -> Jie
  | "jio" -> Jio
  | _ -> failwith "Illegal instruction"

let reg_of_string s = 
  match s with 
  | "a" -> A
  | "b" -> B
  | _ -> failwith "Illegal register"

let drop_last_character str =
  if String.length str <= 1 then
    ""  (* Return an empty string if the input string has length 0 or 1 *)
  else
    String.sub str ~pos:0 ~len:(String.length str - 1)

let program_line_of_string s =
  let parts = String.split ~on:' ' s in
  let instr = instr_of_string (List.hd_exn parts) in
  let offset = 
    match instr with
    | Jmp | Jie | Jio -> int_of_string (List.last_exn parts)
    | _ -> 1
  in
  let reg = 
    match instr with 
    | Hlf | Tpl | Inc -> reg_of_string (List.nth_exn parts 1)
    | Jie | Jio -> reg_of_string (drop_last_character (List.nth_exn parts 1))
    | _ -> A
  in
  { instr; reg; offset }

let a = ref 0
let b = ref 0

let program = Array.of_list_map day23Input ~f:(fun line -> program_line_of_string line)

let halfReg reg = 
  match reg with 
  | A -> a := !a / 2
  | B -> b := !b / 2

let triple reg = 
  match reg with 
  | A -> a := !a * 3
  | B -> b := !b * 3

let incReg reg = 
  match reg with 
  | A -> a := !a + 1
  | B -> b := !b + 1

let isEven reg = 
  match reg with 
  | A -> !a mod 2 = 0
  | B -> !b mod 2 = 0

let isOne reg = 
  match reg with 
  | A -> !a = 1
  | B -> !b = 1

let printStats pc = 
  Printf.printf "Pc: %d \t A = %d B = %d\n" pc !a !b

let run program = 
  let rec runInt pc =
    let programLine = program.(pc) in
    let ofs = 
      match programLine.instr with 
      | Hlf -> halfReg programLine.reg;
              programLine.offset
      | Tpl -> triple programLine.reg;
              programLine.offset
      | Inc -> incReg programLine.reg;
              programLine.offset
      | Jmp -> programLine.offset
      | Jie -> if isEven programLine.reg then programLine.offset else 1
      | Jio -> if isOne programLine.reg then programLine.offset else 1
    in
    (* printStats pc; *)

    let newPc = pc + ofs in
    if newPc >= 0 && newPc < Array.length program then runInt newPc else 0

  in runInt 0

let _ = run program

let resultP1 = !b

let _ = a := 1
let _ = b := 0
let _ = run program

let resultP2 = !b

