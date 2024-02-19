open Core

let day20Input = 33100000

let presentsAtHouse n = 
  let rec inner count i =
    if i = n then
       count + i * 10
    else 
      if n mod i = 0 then 
        inner (count + i * 10) (i + 1)
      else
        inner count (i + 1)
      in
    inner 0 1


let rec test highest nAtHighest  n step =
  if n = 1000000 then 0
  else 
    let count = presentsAtHouse n in
    if count > highest then begin
      Printf.printf "House %d got %d presents. Diff is %d\n" n count (n - nAtHighest);
      if count > day20Input then n else
      test count n (n+step) step
    end
  else
    test highest nAtHighest (n+step) step

let resultP1 = test 0 0 720720 840

let presentsAtHouseP2 n = 
  let rec inner count i =
    if i = n then
       count + i * 11
    else 
      if n mod i = 0 && n / i <= 50 then 
        inner (count + i * 11) (i + 1)
      else
        inner count (i + 1)
      in
    inner 0 1

let rec testP2 highest nAtHighest  n step =
  if n = 1000000 then 0
  else 
    let count = presentsAtHouseP2 n in
    if count > highest then begin
      Printf.printf "House %d got %d presents. Diff is %d\n" n count (n - nAtHighest);
      if count > day20Input then n else
      testP2 count n (n+step) step
    end
  else
    testP2 highest nAtHighest (n+step) step

let resultP2 = testP2 0 0 498960 1680

(* Got the starting and step values for P1 and P2 by testing and printing in utop *)
