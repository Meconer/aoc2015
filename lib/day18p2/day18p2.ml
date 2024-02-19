open Core

let filename = "lib/day18p2/day18.input.txt"
(* let filename = "lib/day18p2/day18.example.txt" *)
let day18Input = In_channel.read_lines filename

type pos = {col : int; row : int}
type change =  On | Off 

let width = String.length (List.hd_exn day18Input)
let height = List.length day18Input

let grid = Array.create ~len:(width * height) 0

let coordToIdx pos = pos.col + width * pos.row
let idxToCoord idx = {col = idx mod width; row =  idx / width}

let toggle n = 
  if n = 1 then 0 else 1

let corners = [
  { col = 0 ; row = 0};
  { col = width - 1 ; row = 0};
  { col = 0 ; row = height - 1};
  { col = width - 1 ; row = height - 1};
]

let changePos grid col row ch  =
  let newVal = 
    if  List.exists corners ~f:(fun p -> p.col = col && p.row = row ) then On 
    else ch in
    let idx = coordToIdx {col;row} in
    match newVal with 
    | On -> grid.(idx) <- 1
    | Off -> grid.(idx) <- 0

let setGridLine grid lineNo line = 
  let chars = String.to_list line in
  List.iteri chars ~f:(fun col c ->
    if Char.equal c '#' then changePos grid col lineNo On
    else changePos grid col lineNo Off;
  )

let processLines grid lines = 
  let _ = List.iteri lines ~f:( fun lineNo line -> 
    Printf.printf "%s\n" line;
    setGridLine grid lineNo line ) in ()
  

let getNeighbours col' row' =
  let neighbours = [ {col = col' - 1; row = row' - 1 };
    { col = col'; row = row' - 1 };
    { col = col' + 1; row = row' - 1 };
    { col = col' - 1; row = row' };
    { col = col' + 1; row = row' };
    { col = col' - 1; row = row' + 1 };
    { col = col' ; row = row' + 1 };
    { col = col' + 1; row = row' + 1 };
  ]  in
  List.filter neighbours ~f:(fun pos -> pos.col >= 0 && pos.col < width && pos.row >= 0 && pos.row < height)

let countNeighbours colNo rowNo =
    let neighbours = getNeighbours colNo rowNo in
    let valsAtNeighbours = List.map neighbours ~f:(
      fun pos -> 
        let idx = coordToIdx pos in 
        grid.(idx)
        ) 
      in
    List.fold valsAtNeighbours ~init:0 ~f:(+)


let rec doCols colNo rowNo newGrid = 
  (* Printf.printf "%d\n" colNo; *)
  if colNo = width then ()
  else begin
    let count = countNeighbours  colNo rowNo in
    let currVal = grid.(coordToIdx {col = colNo; row = rowNo}) in
    let newVal = 
      if currVal = 1 then 
        if count = 2 || count = 3 then On else Off
      else
        if count = 3 then On else Off in
    let _ = changePos newGrid colNo rowNo newVal in
    let _ = doCols (colNo + 1) rowNo newGrid in
    ()
  end

let copyArray grid newGrid = 
  Array.iteri newGrid ~f:(fun idx elt -> grid.(idx) <- elt );
  ()

let doIteration n = 

  let rec doRows rowNo newGrid =
    (* Printf.printf "%d\n" rowNo; *)
    if rowNo = height then begin
      let _ = copyArray grid newGrid in
      ()
    end
    else begin
      let _ = doCols 0 rowNo newGrid in
      doRows (rowNo + 1)  newGrid
    end in

  if n = 0 then ()
  else begin
    let newGrid = Array.create ~len:(width * height) 0 in
    let _ = List.iter corners ~f:(fun p -> changePos newGrid p.col p.row On)

    in doRows 0 newGrid
  end
    

let rec doIterations remaining =
  let _ = doIteration 1 in
  if remaining = 1 then () else let _ = doIterations (remaining - 1) in
  ()

let _ = processLines grid day18Input

let _ = doIterations 100
let resultP2 = Array.fold ~init:0 ~f:(+) grid 

