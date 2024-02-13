open Core
let filename = "lib/day17/day17.input.txt"

let day17Input = In_channel.read_lines filename

let rec processInput acc  = function
  | [] -> List.rev acc
  | h :: t -> processInput ((int_of_string h)::acc) t

let buckets = processInput [] day17Input

let rec bucketFill  acc buckets remaining = 
  match buckets with 
  | [] -> if remaining = 0 then [acc] else []
  | hd :: tl -> let resWithout = bucketFill   acc tl remaining in   (* Without using this bucket *) 
                let resWith = bucketFill  (hd::acc) tl (remaining - hd) in
                match resWithout, resWith with 
                | [], [] -> []
                | wt , [] -> wt
                | [], wo -> wo
                | wt, wo -> wt @ wo

let r = bucketFill  [] buckets 150

let resultP1 = List.length r

let shortest = List.fold ~init:Int.max_value ~f:(min) (List.map ~f:(List.length ) r)

let listsWithShortestLength = List.filter ~f:(fun l -> shortest = List.length l) r

let resultP2 = List.length listsWithShortestLength
