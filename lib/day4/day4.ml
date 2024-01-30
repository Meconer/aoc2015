open Core
let filename = "lib/day4/day4.input.txt"
let day4Input = In_channel.read_all filename
(* let day4Input = "pqrstuv" *)

let findResult s startString = 
  let rec inner number =
    (* let _ = Printf.printf "%d\n" number in *)
    let sToHash = s ^ (string_of_int number) in
    let hash = Md5.to_hex (Md5.digest_string sToHash) in
    let start = String.sub ~pos:0 ~len:(String.length startString) hash in
    if ( String.equal start startString) then number
    else inner ( number + 1 )
  in inner 0

let resultP1 = findResult day4Input "00000"
let resultP2 = findResult day4Input "000000"