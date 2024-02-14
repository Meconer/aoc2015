let limit = 5
let rec test n = 
  if n = limit then 
    (* Printf.printf "%d " n; *)
    ()
  else 
    test (n + 1)