open Core
let filename = "lib/day5/day5.input.txt"
let day5Input = In_channel.read_lines filename

let countVowels s = 
  let vowels = String.to_list "aeiou" in
  let limit = String.length s in
  let rec inner idx count =
    if idx = limit then count 
    else begin
      if List.exists ~f:(fun c -> Char.equal c s.[idx] ) vowels then
      inner (idx + 1) (count + 1)
      else inner (idx + 1) count
    end
  in inner 0 0

let hasTwoLettersInARow s = 
  let limit = String.length s - 1 in
  let rec inner idx =
    if idx = limit then false
    else begin if  Char.equal s.[idx] s.[idx + 1]
      then true
      else inner (idx + 1)
      end
  in inner 0

let hasIllegalSeqs s = 
   let illegalSeqs = ["ab";"cd";"pq";"xy"] in
   List.exists ~f:(fun el -> String.is_substring s ~substring:el)
     illegalSeqs

let hasAtLeastTreeVowels s = 
  let c = countVowels s in
  c >= 3

let isNice s = 
  if hasIllegalSeqs s then false
  else (hasAtLeastTreeVowels s) && (hasTwoLettersInARow s)

let resultP1 = List.length( List.filter day5Input ~f:(isNice) )

let rec firstTwoExistInRest s =
  if String.length s < 4 then false
  else begin
    let firstTwo = String.sub ~pos:0 ~len:2 s in
    let theRest = String.sub ~pos:2 ~len:(String.length s -2) s in
    if String.is_substring theRest ~substring:firstTwo then
      true
    else
      firstTwoExistInRest (String.sub ~pos:1 ~len:(String.length s - 1) s)
  end

let rec hasRepeatWithOneCharacterInBetween s =
  if String.length s < 3 then false
  else begin
    if (Char.equal s.[0] s.[2])  then true
    else 
      hasRepeatWithOneCharacterInBetween (String.sub ~pos:1 ~len:(String.length s - 1) s)
  end

let isNiceP2 s = 
  hasRepeatWithOneCharacterInBetween s && firstTwoExistInRest s

let resultP2 = List.length( List.filter day5Input ~f:(isNiceP2) )
