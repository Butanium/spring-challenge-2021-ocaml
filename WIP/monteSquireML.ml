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
let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr;;
let copyInPlace model cont = Array.iteri (fun i x -> cont.(i) <- x) model;;

let f = float_of_int;;
module IntSet = Set.Make(Int);;
type mutableTree = {pos:int; mutable size : int; mutable owner : int; mutable isdormant : bool};;
let makeSeed p owner = {pos=p; size=0; owner; isdormant=true};;

type action = GROW of int | COMPLETE of int | WAIT |SEED of int*int;;
type gameState = {activePlayer : int; opponentActive : bool; day : int; playerScores : int*int;
        playerSuns : int*int; nutrients : int};;
type mctNode = ROOT of rootNode | N of rootNode * actionNode and
rootNode  = {mutable visit : float; mutable win : float; mutable unexplored : action RndQ.t; mutable sons : mctNode list;  game : gameState;}
and actionNode = {action : action; mutable father : mctNode;};;

let getT i t = if i = 0 then fst t else snd t;;
let (++) (x1,y1) (x2,y2) = x1 + x2, y1 + y2;;
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
let treeArrayPO = Array.copy treeArray and playerTreesPO = Array.copy playerTrees in
let initPOArrays() = copyInPlace treeArray treeArrayPO; copyInPlace playerTrees playerTreesPO in
(** [getPlayerTreeCount player playerTrees] *)
let getPlayerTreeCount player playerTrees = Array.fold_left (+) 0 playerTrees.(player)
and getTreeOpt t = let tree = treeArray.(t) in if tree.size>=0 then Some tree else None
and getTree t = treeArray.(t)
and getPlayerTrees player = Array.fold_left (fun acc x -> if x.owner=player then x::acc else acc) [] treeArray
in
(** [getPlayerActiveTrees treeArray player] *)
let getPlayerActiveTrees treeArray player =
    Array.fold_left (fun acc x -> if x.owner=player && not x.isdormant then x::acc else acc) [] treeArray
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

let getPotentialAllyShadow tree player =
    let rec aux (pos:int) dir depth acc =
        let n =  (getNeighbours pos).(dir) in
        if n = -1 || depth > 3 then acc else (
            aux n dir (depth + 1) @@
            match getTreeOpt n with
            (* if t.size < depth then less malus *)
            | Some t when t.owner = player -> acc +. 1. /. (1. +. 2. *. f( max 0 (depth - t.size)))
            | _ -> acc
        )
    and loop i acc = if i = 6 then acc else
        loop (i+1) (acc +. aux tree i 1 0.)
    in -. exp (loop 0 0. /. 2.) /. 5. +. 1. /. 5. in

let getNextDir day = (day + 1) mod 6 and
oppDir dir = (dir + 3) mod 6
and getDir day = day mod 6 in
let rec origin pos dir =
    match (getNeighbours pos).(oppDir dir) with
        | -1 -> pos
        | p -> origin p dir
in
(**[getTreeRowIncomes tree dir player]*)
let getTreeRowIncomes tree dir player=
    let shadows = Array.make 7 0 in
    let addShadows depth size =
        for k = depth+1 to min (depth+size) 6 do
            shadows.(k) <- max shadows.(k) size
        done
    in
    let incomes depth tree = addShadows depth tree.size;
        (if tree.owner = player then 1 else -1) * if tree.size > shadows.(depth) then tree.size else 0
    in
    let rec aux pos depth acc = if pos = -1 then acc else
        aux (getNeighbours pos).(dir) (depth + 1) @@
        if pos = tree.pos then acc + incomes depth tree
        else match getTreeOpt pos with
               | Some t -> acc + incomes depth t
               | _ -> acc
    in aux (origin tree.pos dir) 0 0
in
(** [getDiff treeInit newTree turns day player] *)
let getDiff treeInit newTree turns day player =
    let newI dir = getTreeRowIncomes newTree dir player and
    oldI dir = getTreeRowIncomes treeInit dir player in
    let diff i = newI (getNextDir @@ day + i) - oldI (getNextDir @@ day + i) in
    let rec loop i acc = if i = turns then acc else
        loop (i + 1) (acc + diff i)
    in loop 0 0
in

(** [getDiffF treeInit newTree turns day player] *)
let getDiffF treeInit newTree turns day player =
    f (getDiff treeInit newTree turns day player)
in

let getPrice sizeArrays =
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

and getSeedScore player day = function
    | SEED (x,y) -> 0.01 *. exp (5. *.(f day /. maxDayf)**1.7) *. f(richBonus y) +. 2. *.getPotentialAllyShadow y player
    | err -> raise@@  Invalid_argument (toString err)
in

let isPossible sun sizeArrays action =
    (match getTarget action with
    | None -> true
    | Some t -> not (getTree t).isdormant) && getPrice sizeArrays action <= sun
and canPay player sun action = getPrice playerTrees.(player) action <= sun
in
(** [getBestPossibleSeed activeTrees enemySeed player day] *)
let getBestPossibleSeed activeTrees enemySeed player day=
        let test x = match enemySeed with | None -> false | Some t -> t=x in
        let rec aux = function
            | [] -> (WAIT, neg_infinity), (WAIT, neg_infinity)
            | t :: ts -> List.fold_left
                (fun (((_, accS) as acc),((_, accS2) as acc2)) x -> if richness x > 0 && (test x || getTreeOpt x = None) then
                let seed = SEED (t.pos, x) in let score = getSeedScore player day seed in (if score > accS then (seed, score) else acc)
                , (if richness x = 3 && score > accS2 then (seed, score) else acc2)
                else acc, acc2)
                (aux ts) @@ getNearTiles t.pos t.size
        in match aux activeTrees with
            | (WAIT, _), _ -> None, None
            | (a, _), (WAIT, _) -> Some a, None
            | (a,_), (b, _) -> Some a, Some b

in
let getMCTSActions player oldSeed sun treeArray playerTrees day=
    if sun = 0 then (
        if playerTrees.(player).(0) > 0 then RndQ.create 1 [|WAIT|]
        else  (
        let playerActiveTrees = getPlayerActiveTrees treeArray player in
            match getBestPossibleSeed playerActiveTrees oldSeed player day with
                | None, _ -> RndQ.create 1 [|WAIT|]
                | Some a, None -> RndQ.create 2 [|WAIT;a|]
                | Some a, Some b -> RndQ.create 3 [|WAIT; a; b|]
        )
    )
    else begin
        let playerActiveTrees = getPlayerActiveTrees treeArray player in
        let r = Array.make (3 + getPlayerTreeCount player playerTrees) WAIT in
        let rec aux i = function
            | [] -> if sun >= playerTrees.(player).(0) then begin
                        match getBestPossibleSeed playerActiveTrees oldSeed player day with
                            | None, _ -> i
                            | Some a, None -> r.(i) <- a; i+1
                            | Some a, Some b -> r.(i) <- a; r.(i+1) <- b; i+2
                        end else i
            | t :: ts -> let action = if t.size < 3 then GROW t.pos else COMPLETE t.pos
                in if canPay player sun action then (r.(i) <- action; aux (i+1) ts) else aux i ts;
        in let size = aux 0 playerActiveTrees in RndQ.create (size+1) r
    end
in
(** ________________Monte Carlo Tree Search ________________**)

(**[performAction playerId treeArray playerTrees]*)
let performAction playerId treeArray playerTrees = function
    | WAIT -> ()
    | GROW t -> let tree = treeArray.(t) in tree.size <- tree.size + 1; tree.isdormant <- true;
        playerTrees.(playerId).(tree.size) <- playerTrees.(playerId).(tree.size) - 1;
        playerTrees.(playerId).(tree.size+1) <- playerTrees.(playerId).(tree.size + 1) + 1;
    | SEED (s, e) -> let treeS, treeE = treeArray.(s), treeArray.(e) in treeS.isdormant <- true; treeE.isdormant <- true;
        if treeE.size > 0 then failwith "plant on another tree" else begin
            if treeE.size = 0 then (
                assert(playerId=1);
                treeE.size <- -1; playerTrees.(1-playerId).(0) <- playerTrees.(1-playerId).(0) - 1;
            ) else (
                treeE.size <- 0; treeE.owner <- playerId;
                playerTrees.(playerId).(0) <- playerTrees.(playerId).(0) + 1;
            )
        end
    | COMPLETE t -> treeArray.(t).size <- -1; playerTrees.(playerId).(3) <- playerTrees.(playerId).(3) - 1;
and undoAction playerId = function
    | WAIT -> ()
    | GROW t -> let tree = treeArray.(t) in tree.size <- tree.size - 1; tree.isdormant <- false;
        playerTrees.(playerId).(tree.size) <- playerTrees.(playerId).(tree.size) - 1;
        playerTrees.(playerId).(tree.size-1) <- playerTrees.(playerId).(tree.size - 1) + 1;
    | SEED (s, e) -> let treeS, treeE = treeArray.(s), treeArray.(e) in treeS.isdormant <- false; treeE.isdormant <- false;
        if treeE.size = 0 then (
            treeE.size <- -1; playerTrees.(playerId).(0) <- playerTrees.(playerId).(0) - 1
        ) else (
            assert (playerId = 1 && treeE.size = -1)
        )
    | COMPLETE t -> let tree = treeArray.(t) in tree.size <- 3; tree.owner <- playerId;
        playerTrees.(playerId).(3) <- playerTrees.(playerId).(3) + 1;
and dayReset = Array.iter (fun x -> x.isdormant <- false) treeArray
and resetTree t = let tree = treeArray.(t) in tree.size <- -1
and updateTree t model = let tree = treeArray.(t) in tree.size <- model.size; tree.owner <- model.owner;
    tree.isdormant <- model.isdormant;
in
let refreshGame = Array.iteri (fun i _ ->
        match getTreeOpt i with
            | None -> resetTree i
            | Some t -> updateTree i t) treeArray
and random = function | [] -> raise Not_found | x :: _ -> x
and getRoot = function | ROOT r | N (r,_) -> r
and getFather = function | ROOT _ -> raise (Invalid_argument "no father for root") | N (_,x) -> x.father
in
let getAction = function | ROOT _ -> failwith "no action for root" | N (_, {action;_}) -> action in
let shadowArray = Array.make numberofcells (0) in
let addShadow tree dir =
    let rec aux pos depth = if depth <= tree.size then (
        let p = (getNeighbours pos).(dir) in shadowArray.(p) <- max shadowArray.(p) tree.size;
        aux p (depth + 1)
     )
    in aux tree.pos 1
in

let updateShadow dir = mapInPlace (fun _ -> 0) shadowArray;
    Array.iter (fun t -> if t.size > 0 then addShadow t dir) treeArray;
in
let getIncomes day = updateShadow (getDir day);
    let (a1,a2), _ = Array.fold_left (fun ((acc1,acc2),i) t ->(if t.size > shadowArray.(i)
        then (if t.owner = 0  then t.size+acc1, acc2 else acc1, t.size+acc2) else acc1,acc2), i+1) ((0,0),0) treeArray
    in a1, a2
in

let ucb1 node = let r = getRoot node and nf =(getRoot (getFather node)).visit in
     r.win /. r.visit +. (2. *. log nf /. r.visit)**0.5
in
let chooseNode = function
    | [] -> failwith "Nil"
    | x :: xs ->
        let rec aux accS accList accLen = function
            | x :: xs -> let s = ucb1 x in let ax = if s > accS then aux s [x] 1 else (
                    if s = accS then aux accS (x :: accList) (accLen+1) else aux accS accList accLen
                ) in ax xs
            | [] -> accList, accLen
        in let l, size = aux (ucb1 x) [x] 1 xs in List.nth l (Random.int size)
in
let getWinner (sun1, sun2) (score1, score2) playerTrees =
    let s = (1. +. f (compare (sun1/3 + score1) (sun2/3 + score2))) /. 2. in
    if s = 0.5 then (1. +. f (compare (getPlayerTreeCount 0 playerTrees) (getPlayerTreeCount 1 playerTrees))) /. 2. else s
in
(** playout node oldSeed oldComplete *)
let playout node oldSeed oldComplete =
    initPOArrays();
    let nodeGame = (getRoot node).game in
    let initPlayer = nodeGame.activePlayer in
    (* aux game oldSeed oldA *)
    let rec aux game oldSeed oldA =
        let p = game.activePlayer in
        let actions = getMCTSActions p oldSeed (getT p game.playerSuns) treeArrayPO playerTreesPO game.day in
        let action = RndQ.take actions in
        let activePlayer, opponentActive, day, playerSuns =
            if action = WAIT then (
                if game.opponentActive then 1 - game.activePlayer, false, game.day, game.playerSuns
                else 0, true, game.day+1, game.playerSuns ++ if game.day > maxDay then 0, 0
                    else getIncomes (game.day+1) (* next game day *)
            ) else (
                (if game.opponentActive then 1 - game.activePlayer else game.activePlayer),
                game.opponentActive, game.day, game.playerSuns
            )

        in if day > maxDay then (
            let w = getWinner playerSuns game.playerScores playerTreesPO in if initPlayer = 0 then w else 1. -. w
        ) else (
            let bonus, nutrients =
                match action with
                    | COMPLETE t -> let nut = game.nutrients + if oldComplete then 1 else 0
                        in (richBonus t) + nut, game.nutrients-1
                    | _ -> 0, game.nutrients
            in
            let playerScores = game.playerScores ++ if activePlayer=0 then bonus,0 else 0,bonus in
            let oldS = match action with | SEED (_, y) when activePlayer = 1 && opponentActive -> Some y | _ -> None
            in
            let oldC = match action with | COMPLETE _ -> activePlayer = 1 && opponentActive | _ -> false in
            performAction activePlayer treeArrayPO playerTreesPO action;
            aux {activePlayer; opponentActive; day; playerScores; playerSuns; nutrients} oldS oldC
        )
    in aux nodeGame oldSeed oldComplete
in



let rec retropropagation win = function
    | ROOT r -> r.visit <- r.visit +. 1.; r.win <- r.win +. win;
    | N (r,{action; father}) -> r.visit <- r.visit +. 1.;
        r.win <- r.win +.  win; undoAction r.game.activePlayer action;
            retropropagation (if r.game.activePlayer = (getRoot father).game.activePlayer then win else 1. -. win) father;
in

(* [expend node]*)
let expend : mctNode -> unit = fun node ->
    let game = match node with | N({game; _},_) | ROOT {game; _} -> game in
    let r = getRoot node in
    let activePlayer, opponentActive, day, initPlayerSuns =
        match node with
            | N (_, {action=WAIT;_}) ->
                    if game.opponentActive then
                        1 - game.activePlayer, false, game.day, game.playerSuns
                    else 0, true, game.day+1, game.playerSuns ++ if game.day > maxDay then 0, 0
                    else getIncomes (game.day+1) (* next game day *)
            | ROOT _ -> game.activePlayer, game.opponentActive, game.day, game.playerSuns
            | N _ -> (if game.opponentActive then 1 - game.activePlayer else game.activePlayer),
                            game.opponentActive, game.day, game.playerSuns
    in

    let action = RndQ.take r.unexplored in
    let trees = playerTrees.(activePlayer) in
    let playerSuns = initPlayerSuns ++ (if activePlayer=0 then -getPrice trees action, 0 else 0, -getPrice trees action) in
    assert (let x, y = playerSuns in x>=0 && y>=0);
    let oldA = (match node with | N (_, {action=SEED _ as a}) | N (_, {action=COMPLETE _ as a}) -> Some a | _ -> None)
    in
    let bonus, nutrients =
        match action with
            | COMPLETE t -> let nut = game.nutrients +
                (match oldA with | Some (COMPLETE _) when activePlayer=1 && game.activePlayer=0-> 1 | _ -> 0)
                in (richBonus t) + nut, game.nutrients-1
            | _ -> 0, game.nutrients
    in
    let playerScores = game.playerScores ++ if activePlayer=0 then bonus,0 else 0,bonus
    in
    performAction activePlayer treeArray playerTrees action;
    let oldSeed = match oldA with Some SEED (x, y) when activePlayer = 1 && game.activePlayer=0 -> Some y | _ -> None in
    let game = {activePlayer; opponentActive; day; playerScores; playerSuns; nutrients} and
        unexplored = getMCTSActions activePlayer oldSeed (getT activePlayer playerSuns) treeArray playerTrees day
    in
    let newNode = N ({visit=0.;win=0.; unexplored; sons=[]; game}, {action; father=node}) in
    r.sons <- newNode :: r.sons;
    let currentSeed = match action with | SEED (x, y) when activePlayer = 0 && game.opponentActive -> Some y | _ -> None
    in
    let currentComplete = match action with | COMPLETE _ -> activePlayer = 0 && game.opponentActive | _ -> false in
    retropropagation
    (if day > maxDay then getWinner playerSuns playerScores playerTrees
    else playout newNode currentSeed currentComplete) node

in

let rec selection : mctNode -> unit = fun n ->
    (match n with
        | ROOT _ -> ()
        | N ({game={activePlayer;_};_}, {action;_}) ->  performAction activePlayer treeArray playerTrees action);
    let r = getRoot n in
    if RndQ.is_empty r.unexplored then selection @@ chooseNode r.sons
    else expend n
in


let monteCarloSearch eval game actionMod initActions limitTime=
    let firstAction = WAIT in
    let mcTree = ROOT {unexplored=initActions; sons=[]; visit=0.; win=0.;game} in
    while Sys.time () < limitTime do
        selection mcTree;
    done;
    match (getRoot mcTree).sons with
        | [] -> failwith "empty sons"
        | N ({visit;win;_}, {action;_}) :: xs -> let acRes, winRate = List.fold_left (fun ((accAction, winrate) as acc) -> function
            | N ({visit;win;_}, {action;_}) -> let ratio = win/.visit in if ratio > winrate then action, ratio else acc
            | _ -> failwith "Root in root sons")  (action, win /. visit)  xs in prerr_endline @@
                Printf.sprintf "Node win ratio : %f, action : %s" winRate (toString acRes); acRes, winRate
        | _ -> failwith "Root in root sons"
in










(** ________________ End ________________ **)



(** ________________ Heuristic ________________ **)
let advantage sizeCount =
    let rec loop acc i =
        if i = 4 then acc else loop (acc + sizeCount.(i)*(i+1)) (i+1)
    in loop 0 0
in




let heursitic gameState=
    let day = gameState.day in let dayf = f day
    and sun = getT gameState.activePlayer gameState.playerSuns
    in
    let player = gameState.activePlayer in
    let myTrees = getPlayerTrees player and
    sizeCount = playerTrees.(player) in
    let possible = isPossible sun sizeCount and
    aM, aE = advantage playerTrees.(0), advantage playerTrees.(1) in
    let advDebug = Printf.sprintf "%d/%d" (aM) (aE) in
    let enTreeRatio = f aE /. f aM in
    let getScore a = let s1,s2 = (
        match a with
        | COMPLETE t -> let nb3 = sizeCount.(3) in -. 4. +. 20. ** ((dayf/. maxDayf)**2.) +.
            (if nb3>2 then f nb3 else 0.)+. (if day=maxDay then 10. else (min 0. (4. *.(1. -. enTreeRatio))))
            +. 2. +.  getDiffF (getTree t) (makeSeed t player) (min 2 (maxDay-day)) day player +. max 0. (f sun -. 5.),
            f (richBonus t)
        | SEED (x,y) -> if maxDay - day < 3 then -1. , 0. else (if day <= 1 then 0. else 10.) *. (exp (-.2.4 *.(f sizeCount.(0))**2.))
        , 0.01 *. exp (5. *.(dayf /. maxDayf)**1.7) *. f(richBonus y) +. 2. *.getPotentialAllyShadow y player
        | GROW t ->let tree = getTree t in let size = tree.size in if maxDay - day + size < 3 then -1. , 0. else 5. +. 2. *.(f size -. 1.5)*.dayf/.maxDayf -. f sizeCount.(size+1)
         +. 2. +. getDiffF tree ({pos=t; size=size + 1; owner=player; isdormant=true}) (min 2 (maxDay-day)) day player , f (richBonus t)
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
            in WAIT :: aux myTrees
    in
    let compareScore = fun (xs1, xs2) (ys1, ys2) -> 2*compare xs1 ys1 + compare xs2 ys2 > 0 in
    match myActions with | [] -> raise Not_found | x :: xs ->
        let chosenAction, _ = List.fold_left (fun (accA,accS) x -> let score = getScore x in
            if  compareScore score accS then (x, score) else (accA, accS)) (x, getScore x) myActions
        in if possible chosenAction then chosenAction else WAIT
in

(** [getGameState oppActive day myScore oppScore mySun oppSun nutrients] *)
let getGameState oppActive day myScore oppScore mySun oppSun nutrients=
    {activePlayer=0; opponentActive = oppActive; day; playerScores=myScore, oppScore; playerSuns=mySun, oppSun;
    nutrients}
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

    let oppsun, oppscore, opponentActive = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun oppsun oppscore oppiswaiting -> (oppsun, oppscore, oppiswaiting = 0)) in
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
    prerr_endline @@ "tree advantage : "^advDebug;
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let rec aux n = if n = numberofpossiblemoves then () else let _ = read_line() in aux (n+1)
    in aux 0;


   let aM, aE = advantage playerTrees.(0), advantage playerTrees.(1) in
   let advDebug = Printf.sprintf "%d/%d" (aM) (aE) in
        let comment = Printf.sprintf "time: %f " (Sys.time() -. startTime) ^ advDebug in
    let x = heursitic @@ getGameState opponentActive day score oppscore sun oppsun nutrients in
    print_endline @@ toString x
    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;