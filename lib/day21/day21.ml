open Core

let filename = "lib/day21/day21.input.txt"
let day21Input = In_channel.read_lines filename

type state = {
  hitPoints : int;
  damage : int;
  armor : int;
}

type item = {
  name : string;
  cost : int;
  damage : int;
  armor : int;
}

type winner = You | Boss 

(* Boss hp set by my input on AoC *)
let boss = {hitPoints = 104; damage = 8; armor = 1}

let rec playGame (boss : state) (you : state) = 
  let bossDamage = max 1 (you.damage - boss.armor) in
  let newBossHp = boss.hitPoints - bossDamage in
  (* Printf.printf "Boss hp - %d\n" newBossHp; *)
  if newBossHp <= 0 then You
  else
    let yourDamage = max 1 (boss.damage - you.armor) in
    let newYouHp = you.hitPoints - yourDamage in
    (* Printf.printf "Your hp - %d\n" newYouHp; *)
    if newYouHp <= 0 then Boss
    else
      let newBoss = {hitPoints = newBossHp; damage = boss.damage; armor=boss.armor} in
      let newYou = {hitPoints = newYouHp; damage = you.damage; armor = you.armor} in
      playGame newBoss newYou

let getItemFromLine line = 
  let parts = List.filter ~f:(fun s -> not (String.is_empty s)) (String.split ~on:' ' line) in
  let item = {
      name = List.hd_exn parts; 
      cost = int_of_string (List.nth_exn parts 1);
      damage = int_of_string (List.nth_exn parts 2);
      armor = int_of_string (List.nth_exn parts 3)} in
      item

let readItemsFromFile lines = 
  let rec inner weapons armors rings section line =
    match line with 
    | [] -> (List.rev weapons, List.rev armors, List.rev rings)
    | hd::tl when String.is_empty hd -> inner weapons armors rings (section+1) tl
    | hd::tl ->
      match section with 
      | 0  -> let item = getItemFromLine hd in
         inner (item::weapons) armors rings section tl
      | 1  -> let item = getItemFromLine hd in
         inner weapons (item::armors) rings section tl
      | 2  -> let item = getItemFromLine hd in
         inner weapons armors (item::rings) section tl
      | _ -> failwith "Bad input"
  in inner [] [] [] 0 lines


let (weapons, armors, rings) = readItemsFromFile day21Input 



let rec pickCombo (lst1 : item list) (lst2 : item list)  =
  match lst1, lst2 with 
  | [], _| _, [] -> []
  | a::atl , _-> 
    let combs_with_lst2 = List.map ~f:(fun b -> [a;b]) lst2 in
    let zero_from_lst2 = [a::([]: item list)] in
    let rest_combs = pickCombo atl lst2 in
    let combs_1_and_2 = combs_with_lst2 @ zero_from_lst2 @ rest_combs in
    combs_1_and_2

let rec pick_1 (lst : item list) =
  match lst with 
  | [] -> []
  | hd::tl -> 
    let withElt = [[hd]] in
    let withoutElt = ([] : item list list ) in
    [] @ withElt @ withoutElt @ pick_1 tl

let rec pick_2 (lst : item list) =
  match lst with 
  | [] -> []
  | _::[] -> []
  | a::b::tl -> 
    let with_2 = [[a;b]] in
    let with_a = List.map tl ~f:(fun elt -> [a;elt]) in
    let with_b = List.map tl ~f:(fun elt -> [b;elt]) in
    with_2 @ with_a @ with_b @ (pick_2 tl)


let ringCombos = pick_2 rings @ pick_1 rings @ [[]]

let armorCombos = pick_1 armors @ [[]]


let rec getMinGold minGold weapons = 
  match weapons with 
  | [] -> minGold
  | weapon::weaponstl -> 
    (* For this weapon do*)
    let results = List.map ringCombos ~f:(
      (*For each ring combo*)
      fun rc -> 
        List.map armorCombos 
          (* For each armor combo*)
          ~f:(fun ac -> 
          let cost =
            weapon.cost 
            + (List.fold ~init:0 ~f:(fun i a -> i + a.cost) ac )
            + (List.fold ~init:0 ~f:(fun i r -> i + r.cost) rc) in
          let dmg = 
            weapon.damage
            + (List.fold ~init:0 ~f:(fun i r -> i + r.damage) rc) in

          let arm = 
              (List.fold ~init:0 ~f:(fun i a -> i + a.armor) ac )
              + (List.fold ~init:0 ~f:(fun i r -> i + r.armor) rc) in

          let playerState = {
            hitPoints = 100;
            damage = dmg;
            armor = arm
            } in

          let winner = playGame boss playerState in
          match winner with 
          | You ->  cost
          | Boss ->  Int.max_value

          )
        ) in
        let fResults = List.concat results in
        let minGoldWithThisWeapon = 
          List.fold ~init:Int.max_value ~f:(min) 
            fResults in
        
        getMinGold (min minGoldWithThisWeapon  minGold)  weaponstl


let rec getMaxGold maxGold weapons = 
  match weapons with 
  | [] -> maxGold
  | weapon::weaponstl -> 
    (* For this weapon do*)
    let results = List.map ringCombos ~f:(
      (*For each ring combo*)
      fun rc -> 
        List.map armorCombos 
          (* For each armor combo*)
          ~f:(fun ac -> 
          let cost =
            weapon.cost 
            + (List.fold ~init:0 ~f:(fun i a -> i + a.cost) ac )
            + (List.fold ~init:0 ~f:(fun i r -> i + r.cost) rc) in

          let dmg = 
            weapon.damage
            + (List.fold ~init:0 ~f:(fun i r -> i + r.damage) rc) in

          let arm = 
              (List.fold ~init:0 ~f:(fun i a -> i + a.armor) ac )
              + (List.fold ~init:0 ~f:(fun i r -> i + r.armor) rc) in

          let playerState = {
            hitPoints = 100;
            damage = dmg;
            armor = arm
            } in

          let winner = playGame boss playerState in
          match winner with 
          | You ->  0
          | Boss -> cost

          )
        ) in
        let fResults = List.concat results in
        let maxGoldWithThisWeapon = 
          List.fold ~init:0 ~f:(max) 
            fResults in
        
        getMaxGold (max maxGoldWithThisWeapon  maxGold)  weaponstl

let resultP1 = getMinGold Int.max_value weapons


let resultP2 = getMaxGold 0 weapons

