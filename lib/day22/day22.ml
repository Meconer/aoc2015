open Core

type bossState = {
  hitPoints : int;
  damage : int;
}

type playerState = {
  mana : int;
  hitPoints : int;
  armor : int;
  shieldTime : int;
  poisonTime : int;
  rechargeTime : int;
}

type spell  = 
  MagicMissile | Drain | Shield | Poison | Recharge

let costOfSpell spell = 
  match spell with 
  | MagicMissile -> 53
  | Drain -> 73
  | Shield -> 113
  | Poison -> 173
  | Recharge -> 229

type winner = Player | Boss 

let string_of_winner w =
  match w with 
  | Player -> "Player"
  | Boss -> "Boss"

let logFile = Out_channel.create "log.txt"

let loggingOn = false 

let logString s = if loggingOn then
  Out_channel.output_string logFile s

let string_of_spell spell = 
  match spell with
  | MagicMissile -> "Magic missile"
  | Drain -> "Drain"
  | Shield -> "Shield"
  | Poison -> "Poison"
  | Recharge -> "Recharge"

let allSpells = [MagicMissile; Drain; Shield; Poison; Recharge]
(* let allSpells = [MagicMissile] *)

let getAffordedSpells mana = 
  let rec keepAfforded (acc : spell list) (spells : spell list) : spell list =
    match spells with 
    | [] -> List.rev acc
    | spell::tl -> if mana >= costOfSpell spell then keepAfforded (spell::acc) tl
                else keepAfforded acc tl
    in
    keepAfforded [] allSpells

let filterPossibleSpellsByEffectTimes player spells = 
  List.filter spells ~f:(fun spell -> match spell with 
              | MagicMissile -> true
              | Drain -> true
              | Shield -> player.shieldTime <= 2
              | Poison -> player.poisonTime <= 2
              | Recharge -> player.rechargeTime <= 2
    )

let handleEffects (player : playerState) (boss : bossState) = 
  let (player, boss) = if player.rechargeTime > 0 then 
    ({player with rechargeTime = player.rechargeTime - 1; mana = player.mana + 101} , boss)
  else 
    (player , boss)
  in
  let (player,boss) = if player.shieldTime > 0 then
    ( { player with shieldTime = player.shieldTime - 1; armor = 7}, boss)
  else
    ( {player with armor = 0}, boss) 
  in
  if player.poisonTime > 0 then 
    ( {player with poisonTime = player.poisonTime - 1 }, {boss with hitPoints = boss.hitPoints - 3})
  else 
    (player, boss)

let doSpell player (boss : bossState) spell = 
    let newMana = player.mana - costOfSpell spell in
      match spell with
    | Shield ->  if player.shieldTime > 1 then failwith "Cannot cast shield spell when already active" else
                  ( {player with shieldTime = 6; mana = newMana}, 
                    boss)
    | Poison -> if player.poisonTime > 1 then failwith "Cannot cast poison spell when already active" else 
                  ( { player with poisonTime = 6; mana = newMana},
                    boss)
    | Recharge -> if player.rechargeTime > 1 then failwith "Cannot cast recharge spell when already active" else 
                  ( { player with rechargeTime = 5; mana = newMana},
                    boss)
    | MagicMissile -> ({player with mana = newMana}, 
                        {boss with hitPoints = boss.hitPoints - 4})
    | Drain -> ( {player with hitPoints = player.hitPoints + 2 ; mana = newMana},
                 { boss with hitPoints = boss.hitPoints - 2 })

let string_of_spells spells = 
  (List.fold (List.rev spells) ~init:"Spells : [ " ~f:(fun s spell -> s ^ string_of_spell spell ^ ", " )) 
  ^ "]\n"
  
let logSpells spells = 
  logString (string_of_spells spells)

let printSpells spells = 
  Printf.printf "%s" (string_of_spells spells)

let string_of_player player = 
  Printf.sprintf "Player - Mana : %d, HP : %d, ShieldTime : %d PoisonTime : %d RechTime %d\n"
    player.mana player.hitPoints player.shieldTime player.poisonTime player.rechargeTime

let logPlayer (player : playerState) = 
  logString (string_of_player player);
  ()
let printPlayer (player : playerState) = 
  Printf.printf "%s" (string_of_player player)
  
let logBoss (boss : bossState) = 
  logString (Printf.sprintf "Boss HP : %d\n" boss.hitPoints)

let printBoss (boss : bossState) = 
  Printf.printf "Boss HP : %d\n" boss.hitPoints

let printStats  player boss accSpells accMana spell=
  printPlayer player;
  printBoss boss;
  printSpells accSpells;
  Printf.printf "Spell: %s - Spent %d\n\n" (string_of_spell spell) accMana;
  ()

let logStats  player boss accSpells accMana spell=
  logPlayer player;
  logBoss boss;
  logSpells accSpells;
  logString (Printf.sprintf "Spell: %s - Spent %d\n\n" (string_of_spell spell) accMana);
  ()

let debugSpells = [ Poison; Recharge; Shield; Poison; Recharge; Shield;Poison; MagicMissile; MagicMissile ]

let getNextDebugSpell debugSpells = 
  let index_ref = ref (-1) in
  let length = List.length debugSpells in
  
  fun () -> if !index_ref < length -1 then begin
        index_ref := !index_ref +1;
        Printf.printf "ir : %d\n" !index_ref;
        Some (List.nth_exn debugSpells !index_ref)
      end
    else 
      None

let nextSpell = getNextDebugSpell debugSpells


let lowestSoFar = ref Int.max_value

let rec playRound (accMana : int) (accSpells : spell list) (boss : bossState) (player : playerState) (spell : spell) part2 = 
  if accMana > !lowestSoFar then 
    []
  else
  (* logString "Players turn\n";
  logString "============\n";
  logStats player boss accSpells accMana spell; *)
    
    (* Player turn *)

    let player = 
      if part2 then 
        {player with hitPoints = player.hitPoints - 1}
      else 
        player
      in

    if player.hitPoints <= 0 then 
      []
    else

    let (player,boss) = handleEffects player boss in
    (* logString "Effects\n";
    logString "-------\n";
    logStats player boss accSpells accMana spell; *)
    
    if boss.hitPoints <= 0 then 
      let () = logString (Printf.sprintf "Player win : Mana = %d\n" accMana) in
      lowestSoFar := accMana;
      (* printSpells accSpells; *)
      [(accMana , accSpells, Player )]
    else

    let (player, boss ) = doSpell player boss spell in
    (* logString "Cast spells\n";
    logString "-------\n";
    logStats player boss accSpells accMana spell; *)

    
    if boss.hitPoints <= 0 then 
      let () = logString (Printf.sprintf "Player win : Mana = %d\n" accMana) in
      lowestSoFar := accMana;
    (* printSpells accSpells; *)
    [(accMana, accSpells, Player )]
    else
    
    (* Boss turn *)
    (* let _ = logString "Boss turn\n" in *)
    (* let _ = logString "=========\n" in *)
    (* logStats player boss accSpells accMana spell; *)

    (* Effects first*)
    let (player,boss) = handleEffects player boss in

    (* logString "Effects\n"; *)
    (* logString "-------\n"; *)
    (* logStats player boss accSpells accMana spell; *)

    if boss.hitPoints <= 0 then 
      let () = logString (Printf.sprintf "Player win : Mana = %d\n" accMana) in
      (* printSpells accSpells; *)
      [(accMana, accSpells, Player)]
    else

    (* And then the boss damage *)
    let damageDelt = max 1 (boss.damage - player.armor) in
    let player = { player with hitPoints = player.hitPoints - damageDelt } in

    (* logString "Boss Damage\n"; *)
    (* logPlayer player; *)
    (* logString " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"; *)

    if player.hitPoints <= 0 then 
      (* let () = logString (Printf.sprintf "Boss win : Mana = %d\n" accMana) in *)
      []
    else

    (* let _ = printPlayer player in *)
    let availableMana = if player.rechargeTime > 0 then player.mana + 101 else player.mana in
    let affordableSpells = getAffordedSpells availableMana in
    (* Printf.printf "Affordable "; *)
    (* printSpells spells; *)
    (*If player doesnt have mana enough then it is a loss*)
    if List.is_empty affordableSpells then 
      [(-1, accSpells, Boss)]
    else

      let possibleSpells = filterPossibleSpellsByEffectTimes player affordableSpells in
      (* Printf.printf "Possible "; *)
      (* printSpells spells; *)
      (* let spellsToTry = match nextSpell () with
        | Some s -> [s]
        | None -> []
      in *)
    
      let results = List.concat_map possibleSpells ~f:(fun sp -> 
          playRound (accMana + costOfSpell sp) (sp::accSpells) boss player sp part2) in
      results
      
let rec findBest accMana bestSpell results = 
  match results with 
  | [] -> (accMana, bestSpell)
  | hd::tl -> let (r,s,_) = hd in
      if ( r < accMana) then 
        findBest r s tl
      else
        findBest accMana bestSpell tl

(* Boss hp set by my input on AoC *)

(* Boss has no armor since our damage is magic :P *)
let myBoss = {hitPoints = 51 ; damage = 9} 
let testBoss1 = {hitPoints = 13; damage = 8}
let testBoss2 = {hitPoints = 14; damage = 8}

let boss = myBoss

let puzzlePlayer = { mana = 500; hitPoints = 50; armor = 0; shieldTime = 0; poisonTime = 0; rechargeTime = 0}

let testPlayer1 = { mana = 250; hitPoints = 10; armor = 0; shieldTime = 0; poisonTime = 0; rechargeTime = 0}

let player = puzzlePlayer

let spellsToTry = match nextSpell () with
  | Some s -> [s]
  | None -> []
      
let results = List.concat_map allSpells ~f:(fun spell -> playRound (costOfSpell spell) [spell] boss player spell false)

let wins = List.filter results ~f:(fun (_, _, winner) -> match winner with
    | Player -> true
    | Boss -> false
)
let (bestCost, _) = findBest Int.max_value [] wins

let resultP1 = bestCost

(* 1242 too high, 1189 too low for P2*)

(* Reset limit *)
let _ = lowestSoFar := Int.max_value
let results = List.concat_map allSpells ~f:(fun spell -> playRound (costOfSpell spell) [spell] boss player spell true) 

let wins = List.filter results ~f:(fun (_, _, winner) -> match winner with
    | Player -> true
    | Boss -> false
)
let (bestCost, _) = findBest Int.max_value [] wins

let resultP2 = bestCost

