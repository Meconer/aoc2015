open Core
(* let filename = "lib/day14/day14.example.txt" *)
let filename = "lib/day14/day14.input.txt"

let day14Input = In_channel.read_lines filename

type reindeerCapability = {
  name : string;
  speed : int;
  duration : int;
  restTime : int;
}

let processInput (lines : string list) : reindeerCapability list =
  let rec inner acc lines =
  match lines with 
  | [] -> List.rev acc
  | line :: tl ->  let parts = String.split ~on:' ' line in
      let name = List.hd_exn parts in
      let speed = int_of_string (List.nth_exn parts 3) in
      let duration = int_of_string (List.nth_exn parts 6) in
      let restTime = int_of_string (List.nth_exn parts 13) in

      inner ({name = name; speed = speed; duration = duration; restTime = restTime }::acc)  tl

  in inner [] lines

let reindeers = processInput day14Input

let distance time  reindeer = 
  let totalDur = reindeer.duration + reindeer.restTime in
  let noCycles = time / totalDur in
  let remainingTimeFlying = min reindeer.duration (time mod totalDur) in
  (noCycles * reindeer.duration + remainingTimeFlying) *  reindeer.speed

let distances time = List.map ~f:(fun r -> distance time r) reindeers

let resultP1 = List.fold ~init:0 ~f:(max) (distances 2503)

let distancesP2 time = List.map ~f:(fun r -> (r.name, distance time r)) reindeers

let leaders = Hashtbl.create ~size:(List.length reindeers) (module String)

let rec getLeaders  time timeLimit =
  if time > timeLimit then ()
  else
    let dists = distancesP2 time in
    let maxDist = List.fold ~init:0 ~f:(fun m nameDist -> max m (snd nameDist)) dists in
    let leaderNames = List.map ~f:(fst) (List.filter ~f:(fun r -> (snd r) = maxDist) dists ) in
    List.iter ~f:( fun n -> 
       let v = Hashtbl.find_or_add leaders n ~default:(fun () -> 0) in
       let _ = Hashtbl.set leaders ~key:n ~data:(v + 1) in ()
      ) leaderNames;
    getLeaders ( time + 1 ) timeLimit

let _ = getLeaders 1 2503;;

let points = Hashtbl.to_alist leaders

let resultP2 = List.fold ~init:0 ~f:(fun a b -> max a (snd b)) points
