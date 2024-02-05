open Core
(* let filename = "lib/day9/day9.example.txt" *)
let filename = "lib/day9/day9.input.txt"

let day9Input = In_channel.read_lines filename

type edge = {v1 : string ; v2 : string ; dist :  int }

let parseLine line : edge = 
  let parts = String.split line ~on:'=' in
  let placesPart = String.split ( String.strip (List.hd_exn parts)) ~on:' ' in
  let dist = int_of_string (String.strip (List.last_exn parts)) in
    {v1 = List.hd_exn placesPart ; v2 =List.last_exn placesPart; dist = dist }

let buildEdgeList lines =
  List.map lines ~f:(parseLine) 

let rec buildVertexList acc lst = 
  match lst with 
  | [] -> Set.to_list acc
  | h :: t -> buildVertexList (Set.add (Set.add acc h.v1) h.v2) t 

let pick_nth n lst = 
  let pickedEl = List.nth_exn lst n in
  let listBefore = List.sub ~pos:0 ~len:(n) lst in
  let listAfter = List.sub ~pos:(n+1) ~len:((List.length lst)-1-n) lst in
  let restOfList = listBefore @ listAfter in
  (pickedEl, restOfList)

let getNeighbours vertex edges = 
  let relevantEdges = List.filter ~f:(fun edge ->
     String.equal edge.v1 vertex || String.equal edge.v2  vertex) edges in
  List.map ~f:(fun edge -> if String.equal edge.v1 vertex then edge.v2 else edge.v1 ) relevantEdges

let getDistanceToNeighbour current neighbour edges =
  let rec find  lst =
    match lst with 
    | [] -> failwith "Neighbour not found"
    | h :: t -> if 
      String.equal h.v1 current 
      && String.equal h.v2 neighbour 
      || String.equal h.v2 current 
      && String.equal h.v1 neighbour then h.dist
    else find t
  in find edges

let edges = buildEdgeList day9Input
let vertices = buildVertexList (Set.empty (module String)) edges

let removeVertex vertexToRemove vertices = 
  List.filter ~f:(fun v -> not (String.equal v vertexToRemove)) vertices

let rec calcShortestPath acc currVertex restOfVertices =
  if List.is_empty restOfVertices then acc
  else 
    List.fold ~init:Int.max_value ~f:(fun dist cv ->
      let restOfRest = removeVertex cv restOfVertices in
      min dist (acc + calcShortestPath (getDistanceToNeighbour currVertex cv edges) cv restOfRest )) restOfVertices


let shortestPaths = List.map vertices ~f:(fun v -> 
  let start = v in
  let rest = removeVertex v vertices in
  calcShortestPath 0 start rest)

let resultP1 = List.fold ~init:Int.max_value ~f:(min) shortestPaths

let rec calcLongestPath acc currVertex restOfVertices =
  if List.is_empty restOfVertices then acc
  else 
    List.fold ~init:0 ~f:(fun dist cv ->
      let restOfRest = removeVertex cv restOfVertices in
      max dist (acc + calcLongestPath (getDistanceToNeighbour currVertex cv edges) cv restOfRest )) restOfVertices


let longestPaths = List.map vertices ~f:(fun v -> 
  let start = v in
  let rest = removeVertex v vertices in
  calcLongestPath 0 start rest)

let resultP2 = List.fold ~init:0 ~f:(max) longestPaths
