open Core

let startNo = 20151125

let getNoAt row col =
  let rec inner r c n =
    if r = row && c = col then
      n
    else
      let nextNo = n * 252533 mod 33554393 in
      if r = 1 then 
        inner (c + 1) 1 nextNo
      else
        inner (r-1) (c+1) nextNo
  in
  inner 1 1 startNo 

let resultP1 = getNoAt 2978 3083

