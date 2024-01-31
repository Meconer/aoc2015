open Core
let filename = "lib/day6/day6.input.txt"
let day6Input = In_channel.read_lines filename

type pos = {x : int; y : int}
type area = {startPos : pos; endPos : pos}
type change =  On | Off | Toggle

let width = 1000
let height = 1000
let grid = Array.create ~len:(width * height) 0

let coordToIdx pos = pos.x + width * pos.y
let idxToCoord idx = {x = idx mod width; y =  idx / width}

let toggle n = 
  if n = 1 then 0 else 1
let changePos x y ch isPart2 =
  let idx = coordToIdx {x;y} in
  if isPart2 then begin
    match ch with 
    | On -> Array.set grid idx ((Array.get grid idx) + 1)
    | Off -> let brightness = max 0 ((Array.get grid idx) - 1) in
            Array.set grid idx brightness
    | Toggle -> Array.set grid idx ((Array.get grid idx) + 2)
      
  end
  else begin
  match ch with 
  | On -> Array.set grid idx 1
  | Off -> Array.set grid idx 0
  | Toggle -> Array.set grid idx  (toggle (Array.get grid idx))
  end

let changeRow y startx endx ch isPart2 =
  let rec inner x = 
    let _ = changePos x y ch isPart2 in 
    if x = endx then ()
    else inner (x + 1) 
  in inner startx

let changeArea area ch isPart2 = 
  let rec inner y =
    let _ = changeRow y area.startPos.x area.endPos.x ch isPart2 in
    if y = area.endPos.y then ()
    else inner (y+1)
  in inner area.startPos.y

let getChangeMethod line = 
  if String.is_substring line ~substring:"toggle" then Toggle
  else if String.is_substring line ~substring:"turn on" then On
  else Off

let getArea line =
  let parts = String.split ~on:',' line in
  let leftParts = String.split ~on:' ' (List.hd_exn parts) in
  let startx = int_of_string (List.last_exn leftParts) in
  let middleParts = String.split ~on:' ' (List.nth_exn parts 1) in
  let starty = int_of_string (List.hd_exn middleParts) in
  let endx = int_of_string (List.last_exn middleParts) in
  let lastParts = List.last_exn parts in
  let endy = int_of_string lastParts in
  {startPos = {x = startx; y = starty } ; endPos = {x = endx; y = endy}}

let processLine line isPart2 = 
  let changeMethod = getChangeMethod line in
  let area = getArea line in
  changeArea area changeMethod isPart2

let _ = List.iter day6Input ~f:(
  fun line -> processLine line false
)
let resultP1 = Array.count ~f:(fun n -> n = 1) grid

let entireArea = {startPos = {x = 0; y = 0}; endPos = {x = width - 1; y = height - 1}}

let _ = changeArea entireArea Off false

let _ = List.iter day6Input ~f:(
  fun line -> processLine line true
)
let resultP2 = Array.fold ~init:0 ~f:(+) grid