(* TODO adjust endGame, we could optimize it *)
(* TODO copy game state when starting a playout *)
module RndQ = struct
    exception Empty
    type 'a t = {mutable size: int; content : 'a array}
    let create size arr = {size; content = arr}
    let is_empty q = q.size = 0
    let take t = if t.size = 0 then raise Empty else
        let i = Random.int t.size in
        let r = t.content.(i) in
        t.content.(i) <- t.content.(t.size - 1);
        t.size <- t.size - 1;
        r
end;;


let f = float_of_int;;
module IntSet = Set.Make(Int);;
type mutableTree = {pos:int; mutable size : int; mutable owner : bool; mutable isdormant : bool};;
let makeSeed p = {pos=p; size=0; ismine=true; isdormant=true};;

type action = GROW of int | COMPLETE of int | WAIT |SEED of int*int;;
type gameState = {activePlayer : int; opponentActive : bool; day : int; playerScores : int array;
        playerSuns : int array; nutrients : int};;

type mtcNode = ROOT of rootNode | N of rootNode * {action : action; mutable father : mtcNode;} and
rootNode  = {mutable visit : float; mutable win : float; mutable unexplored : action RndQ.t; mutable sons : mtcNode list;  game : gameState;};;


let toString = function
    | GROW t -> Printf.sprintf "GROW %d" t
    | COMPLETE t -> Printf.sprintf "COMPLETE %d" t
    | WAIT -> "WAIT"
    | SEED (a,b) -> Printf.sprintf "SEED %d %d" a b
    in

let numberofcells = int_of_string (input_line stdin) in (* 37 *)

 (* create an array which contains all your neighbours index (-1 meaning that you don't have neighbours in this direction) *)
let neighbourTable = Array.make numberofcells [||]

(* create an array which will associate each cell to
its richness *)
and richnessTable = Array.make numberofcells 0  in
(* return the neighbour array *)
let getNeighbours x = neighbourTable.(x)
(* return the richness of the tile *)
and richness x = richnessTable.(x) in

for i = 0 to numberofcells - 1 do
    let index, richness, neighbours = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d  %d  %d" (fun index richness neigh0 neigh1 neigh2 neigh3 neigh4 neigh5 -> (index, richness, [|neigh0; neigh1; neigh2; neigh3; neigh4; neigh5|])) in
    (* fill the dictionnaries *)
    richnessTable.(index) <- richness ;
    neighbourTable.(index) <- neighbours;
done;



let maxDayf = 23. and
maxDay = 23 in

(* ________________ MAP Init ________________*)
let treeArray = Array.init numberofcells (fun i -> {pos = i; size = -1; owner = -1; isdormant = false})
and playerTrees = Array.init 2 (fun _ -> Array.make 4 0) in

let getTreeOpt t = let tree = treeArray.(t) in if tree.size>=0 then Some tree else None
and getTree t = treeArray.(t)
and getPlayerTrees player = Array.fold_left (fun acc x -> if x.owner=player then x::acc else acc) [] treeArray
and getPlayerActiveTrees player =
    Array.fold_left (fun acc x -> if x.owner=player && not x.isdormant then x::acc else acc) [] game.treeArray
in


let getNearTiles pos maxDepth =
    let queue = Queue.create() in
    Queue.add (pos, 0) queue;
    let rec aux ignore acc =
        if Queue.is_empty queue then acc else
        let pos, depth = Queue.take queue in
        if depth > maxDepth then acc else
        aux (IntSet.union (Array.fold_left (fun acc n -> if n >= 0 && not (IntSet.mem n ignore)
                then (Queue.add (n,depth+1) queue; IntSet.add n acc) else acc)
                IntSet.empty @@ getNeighbours pos) (IntSet.add pos ignore)) (if depth <>0 then pos :: acc else acc)
    in aux IntSet.empty []
in





let richBonus t = 2*(richness t - 1) in

let getPotentialAllyShadow tree =
    let rec aux (pos:int) dir depth acc =
        let n =  (getNeighbours pos).(dir) in
        if n = -1 || depth > 3 then acc else (
            aux n dir (depth + 1) @@
            match getTreeOpt n with
            (* if t.size < depth then less malus *)
            | Some t when t.ismine -> acc +. 1. /. (1. +. 2. *. f( max 0 (depth - t.size)))
            | _ -> acc
        )
    and loop i acc = if i = 6 then acc else
        loop (i+1) (acc +. aux tree i 1 0.)
    in -. exp (loop 0 0. /. 2.) /. 5. +. 1. /. 5. in

let getNextDir day = (day + 1) mod 6 and
oppDir dir = (dir + 3) mod 6 in
let rec origin pos dir =
    match (getNeighbours pos).(oppDir dir) with
        | -1 -> pos
        | p -> origin p dir
in

let getTreeRowIncomes tree dir =
    let shadows = Array.make 7 0 in
    let addShadows depth size =
        for k = depth+1 to min (depth+size) 6 do
            shadows.(k) <- max shadows.(k) size
        done
    in
    let incomes depth tree = addShadows depth tree.size;
        (if tree.ismine then 1 else -1) * if tree.size > shadows.(depth) then tree.size else 0
    in
    let rec aux pos depth acc = if pos = -1 then acc else
        aux (getNeighbours pos).(dir) (depth + 1) @@
        if pos = tree.pos then acc + incomes depth tree
        else match getTreeOpt pos with
               | Some t -> acc + incomes depth t
               | _ -> acc
    in aux (origin tree.pos dir) 0 0
in

let getDiff treeInit newTree turns day =
    let newI = getTreeRowIncomes newTree and
    oldI = getTreeRowIncomes treeInit in
    let diff i = newI (getNextDir @@ day + i) - oldI (getNextDir @@ day + i) in
    let rec loop i acc = if i = turns then acc else
        loop (i + 1) (acc + diff i)
    in loop 0 0
in
let getDiffF treeInit newTree turns day =
    f (getDiff treeInit newTree turns day)
in

let getPrice player =
    let sizeArrays = playerTrees.(player) in
    function
    | WAIT -> 0
    | GROW t -> let tree = getTree t in (match tree.size with | 0 -> 1 | 1 -> 3 | 2 -> 7 |_-> failwith "illegal size") + sizeArrays.(tree.size+1)
    | SEED _ -> sizeArrays.(0)
    | COMPLETE _ -> 4
and getTarget = function
    | GROW t -> Some t
    | COMPLETE t -> Some t
    | SEED (t,_) -> Some t
    | WAIT -> None
and getNoneSeedActions trees =
    let rec aux acc = function
        | [] -> acc
        | t :: ts -> aux (if t.size < 3 then GROW t.pos else COMPLETE t.pos) :: acc
    in WAIT :: aux [] trees
and getSeedScore player day = function
    | SEED (x,y) -> if maxDay - day < 3 then -1. , 0. else (if day <= 1 then 0. else 10.)
        *. (exp (-.2.4 *.(f playerTrees.(player).(0))**2.)), 0.01 *. exp (5. *.(dayf /. maxDayf)**1.7) *. f(richBonus y)
        +. 2. *.getPotentialAllyShadow y
    | err -> raise Invalid_argument @@ toString err
in
let getBestSeedAction ?(enemySeed=None) activeTrees =
        let test x = match enemySeed with | None -> false | Some t -> t=x in
        let rec aux = function
            | [] -> WAIT, neg_infinity
            | t :: ts -> List.fold_left
                (fun ((_, accS) as acc) x -> if richness x > 0 && (test x || getTreeOpt x = None) then
                let score = getSeedScore (SEED (t.pos, x)) in if score > accS then (x, score) else acc else acc)
                (aux ts) @@ getNearTiles t.pos t.size
        in match aux activeTrees with
            | WAIT, _ -> None
            | a, _ -> Some a

in
let isPossible sun sizeArrays action =
    (match getTarget action with
    | None -> true
    | Some t -> not (getTree t).isdormant) && getPrice sizeArrays action <= sun
in

(** ________________Monte Carlo Tree Search ________________**)


(* TODO deal with 2 seed turns *)
let performAction playerId = function
    | WAIT -> ()
    | GROW t -> let tree = treeArray.(t) in tree.size <- tree.size + 1; tree.isdormant <- true;
        playerTrees.(playerId).(tree.size) <- playerTrees.(playerId).(tree.size) - 1;
        playerTrees.(playerId).(tree.size+1) <- playerTrees.(playerId).(tree.size + 1) + 1;
    | SEED s, e -> let treeS, treeE = treeArray.(s), treeArray.(e) in treeS.isdormant <- true; treeE.isdormant <- true;
        treeE.size <- 0; treeE.owner <- playerId; playerTrees.(playerId).(0) <- playerTrees.(playerId).(0) + 1;
    | COMPLETE t -> treeArray.(t).size <- -1; playerTrees.(playerId).(3) <- playerTrees.(playerId).(3) - 1;
and cancelAction playerId = function
    | WAIT -> ()
    | GROW t -> let tree = treeArray.(t) in tree.size <- tree.size - 1; tree.isdormant <- false;
        playerTrees.(playerId).(tree.size) <- playerTrees.(playerId).(tree.size) - 1;
        playerTrees.(playerId).(tree.size-1) <- playerTrees.(playerId).(tree.size + 1) + 1;
    | SEED s, e -> let treeS, treeE = treeArray.(s), treeArray.(e) in treeS.isdormant <- false; treeE.isdormant <- false;
        treeE.size <- -1; playerTrees.(playerId).(0) <- playerTrees.(playerId).(0) - 1;
    | COMPLETE t -> let tree = treeArray.(t) in tree.size <- 3; tree.owner <- playerId;
        playerTrees.(playerId).(3) <- playerTrees.(playerId).(3) + 1;
and dayReset = Array.iter (fun x -> x.isdormant <- false) treeArray
and resetTree t = let tree = treeArray.(t) in tree.size <- -1
and updateTree t model = let tree = treeArray.(t) in tree.size <- model.size; tree.owner <- model.ismine;
    tree.isdormant <- model.isdormant;
in
let refreshGame = Array.iteri (fun i _ ->
        match getTreeOpt i with
            | None -> resetTree i
            | Some t -> updateTree i t) treeArray
and random = function | [] -> raise Not_found | x :: _ -> x
and getRoot = function | ROOT r | N (r,_) -> r
and getFather = function | ROOT _ -> raise Invalid_argument "no father for root" | N (_,x) -> x.father
in
let ucb1 node = let r = getRoot node andin
     r.win /. r.visit +. (2. *. log /. r.visit)**.5
in
let chooseNode = function
    | [] -> failwith "Nil"
    | x :: xs ->
        let rec aux accS accList accLen = function
            | x :: xs -> let s = ucb1 x in if s > accS then aux s [x] 1 else (
                    if s = accS then aux accS (x :: accList) (accLen+1) else aux accS accList accLen
                )
            | [] -> accL, accLen
        in let l, size = aux (ucb1 x) [x] 1 in List.nth l (Random.int size)
let expend node = let root = getRoot node in
(*TODO chosen action must influence playerScores, playerSUns and nutrients*)
(*TODO as for seed we have to reduce nutrients 1 times on 2 *)
    let gameState =
        match node with
            | R r -> r.game
            | N ({game;_}, {action;_}) -> if action = WAIT then (
                    if game.opponentActive then {activePlayer = 1 - game.activePlayer; opponentActive=false; day=game.day;
                    playerScore = game.playerScore; playerSuns}
                    else
                ) else r.game.opponentActive
    let action = RndQ.take r.unexplored in

    r.sons <- N ({visit=0.; })
let rec selection n =
    (match n with
        | ROOT _ -> ()
        | N (_, {action;_}) -> performAction action);
    let r = getRoot n in
    if RndQ.is_empty r.unexplored then selection @@ chooseNode r.sons
    else expend n



let monteCarloSearch eval gameState actionMod initActions limitTime=
    let firstAction = WAIT in
    let mcTree = ROOT {unexplored=initActions; sons=[]; visit=0; win=0;lose=0;} in
    while Sys.time () < limitTime do









(** ________________ End ________________ **)



(** ________________ Heuristic ________________ **)
let advantage sizeCount =
    let rec loop acc i =
        if i = 4 then acc else loop (acc + sizeCount.(i)*(i+1)) (i+1)
    in loop 0 0
in




let heursitic gameState player =
    let myTrees = getPlayerTrees player in
    let day = gameState.day
    and dayf = f day
    and sun = gameState.playerSuns.(player)
    and sizeCount = playerTrees.(player)
    in
    let possible = isPossible sun sizeCount and
    aM, aE = advantage playerTrees.(0), advantage playerTrees.(1) in
    let advDebug = Printf.sprintf "%d/%d" (aM) (aE) in
    let enTreeRatio = f aE /. f aM in
    let getScore a = let s1,s2 = (
        match a with
        | COMPLETE t -> let nb3 = sizeCount.(3) in -. 4. +. 20. ** ((dayf/. maxDayf)**2.) +.
            (if nb3>2 then f nb3 else 0.)+. (if day=maxDay then 10. else (min 0. (4. *.(1. -. enTreeRatio))))
            +. 2. +.  getDiffF (getTree t) (makeSeed t) 2 (min 2 (maxDay-day)) +. max 0. (f sun -. 5.),
            f (richBonus t)
        | SEED (x,y) -> if maxDay - day < 3 then -1. , 0. else (if day <= 1 then 0. else 10.) *. (exp (-.2.4 *.(f sizeCount.(0))**2.))
        , 0.01 *. exp (5. *.(dayf /. maxDayf)**1.7) *. f(richBonus y) +. 2. *.getPotentialAllyShadow y
        | GROW t ->let tree = getTree t in let size = tree.size in if maxDay - day + size < 3 then -1. , 0. else 5. +. 2. *.(f size -. 1.5)*.dayf/.maxDayf -. f sizeCount.(size+1)
         +. 2. +. getDiffF tree ({pos=t; size=size + 1; ismine=true; isdormant=true}) (min 2 (maxDay-day)) day , f (richBonus t)
        | WAIT -> 0., 0.

        ) in if possible a then s1,s2 else (
            if (match a with | SEED _ -> true | _ -> false) then (s1, 0.99 *. s2) else (0.5 *. s1, 0.5 *. s2)
        ) in
    let myActions =
        let rec aux = function
                | [] -> []
                | t :: ts -> let seeds = List.fold_left (fun acc x -> if richness x > 0 && getTreeOpt x = None then SEED(t.pos, x):: acc else acc)
                                                        (aux ts) @@ getNearTiles t.pos t.size
                             in if t.size < 3 then GROW t.pos :: seeds else COMPLETE t.pos :: seeds
            in WAIT :: aux trees
    in
    let compareScore = fun (xs1, xs2), (ys1, ys2) -> 2*compare xs1 ys1 + compare xs2 ys2 > 0 in
    match myActions with | [] -> raise Not_found | x :: xs ->
        let chosenAction, _ = List.fold_left (fun (accA,accS) x -> let score = getScore x in
            if  compareScore score accS then (x, score) else (accA, accS)) (x, getScore x) myActions
        in chosenAction
in

(** ________________ getGameState ________________ **)
let getGameState oppActive day myScore oppScore mySun oppSun nutrients=
    {activePlayer=0; opponentActive = oppActive; day=dat; playerScores=[|myScore; oppScore|]; playerSuns=[|mySun; oppSun|]
    nutrients=nutrients}
in

(* game loop *)
while true do
    let startTime = Sys.time () in
    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    let dayDebug = Printf.sprintf "day : %d/%d" day maxDay in
    prerr_endline @@ dayDebug;
    let dayf = f day in
    let nutrients = int_of_string (input_line stdin) in (* the base score you gain from the next COMPLETE action *)

    let sun, score = Scanf.sscanf (input_line stdin) " %d  %d" (fun sun score -> (sun, score)) in

    let oppsun, oppscore, oppiswaiting = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun oppsun oppscore oppiswaiting -> (oppsun, oppscore, oppiswaiting = 1)) in
    let numberoftrees = int_of_string (input_line stdin) in (* the current amount of trees *)

    for i = 0 to 1 do for k=0 to 3 do playerTrees.(i).(k) <- 0 done; done;
    for i = 0 to numberoftrees - 1 do
        let owner, size = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d"
        (fun cellindex size ismine isdormant -> let tree = treeArray.(cellindex) in tree.size <- size; tree.owner <- 1-ismine;
        tree.isdormant <- isdormant=1; 1-ismine, size) in
        playerTrees.(owner).(size) <- playerTrees.(owner).(size) + 1
    done;
    let a x =
        Array.fold_left (+) 0 @@ Array.mapi (fun i x -> (i+1)*x) x in
    let advDebug = Printf.sprintf "%d/%d" (a playerTrees.(0)) (a playerTrees.(1)) in
    let enTreeRatio = f (a oppsizeCount) /. f (a sizeCount) in
    prerr_endline @@ "tree advantage : "^advDebug;
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let rec aux n = if n = numberofpossiblemoves then () else let _ = read_line() in aux (n+1)
    in aux 0 in


   let aM, aE = advantage playerTrees.(0), advantage playerTrees.(1) in
   let advDebug = Printf.sprintf "%d/%d" (aM) (aE) in
        let comment = Printf.sprintf "time: %f " (Sys.time() -. startTime) ^ advDebug in
    let x = heursitic in
    prerr_endline @@ if possible x then  (toString x) ^" "^comment
    else ("WAIT"^(if numberofpossiblemoves>1 then (" WAIT FOR "^(toString x)) else "")^" "^comment)

    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;