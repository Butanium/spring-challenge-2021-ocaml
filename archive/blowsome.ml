(* TODO (2) get shadow diff for complete/grow tree *)
(* TODO (3) optimize plants at the end, we need less seeds and more trees *)
(* TODO (1) seed where there are no allies if you can  *)
let f = float_of_int;;
module IntSet = Set.Make(Int);;
type tree = {pos : int; size : int; ismine : bool; isdormant : bool};;
let makeSeed p = {pos=p; size=0; ismine=true; isdormant=true};;

type action = GROW of int | COMPLETE of int | WAIT |SEED of int*int;;
let action s = match s.[0] with
    | 'W' -> WAIT
    | 'G' ->  GROW (Scanf.sscanf s "%s %d" (fun _ b -> b))
    | 'C' -> COMPLETE (Scanf.sscanf s "%s %d" (fun _ b -> b))
    | 'S' -> let a,b = Scanf.sscanf s "%s %d %d" (fun _ a b -> a,b) in SEED (a,b)
    | _ -> failwith @@ s^" is an invalid action"


(* Convert an action to a string
toString (COMPLETE 3) return "COMPLETE 3" *)
and toString = function (* a good exemple on how you can
match actions *)
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
    (* index: 0 is the center cell, the next cells spiral outwards *)
    (* richness: 0 if the cell is unusable, 1-3 for usable cells *)
    (* neigh0: the index of the neighbouring cell for each direction *)
    let index, richness, neighbours = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d  %d  %d" (fun index richness neigh0 neigh1 neigh2 neigh3 neigh4 neigh5 -> (index, richness, [|neigh0; neigh1; neigh2; neigh3; neigh4; neigh5|])) in
    (* fill the dictionnaries *)
    richnessTable.(index) <- richness ;
    neighbourTable.(index) <- neighbours;
done;

let getIncomes trees = Array.fold_left (+) 0 @@ Array.mapi (fun i x -> x*i) trees in


let maxDayf = 23. and
maxDay = 23 in
let treeTable = Hashtbl.create 37 in
let getTreeOpt x = Hashtbl.find_opt treeTable x and
    getTree x = Hashtbl.find treeTable x in
let getNearTrees pos maxDepth =
    let queue = Queue.create() in
    Queue.add (pos, 0) queue;
    let rec aux ignore =
        if Queue.is_empty queue then [] else
        let pos, depth = Queue.take queue in
        if depth > maxDepth then [] else
        let l = aux @@ IntSet.union (Array.fold_left (fun acc n -> if n >= 0 && not (IntSet.mem n ignore) then
                                 (Queue.add (n,depth+1) queue; IntSet.add n acc)
                                 else acc) IntSet.empty @@ getNeighbours pos)
                                 (IntSet.add pos ignore) in
        match getTreeOpt pos with
        | None -> l
        | Some x -> x::l
    in aux IntSet.empty

(* getVisibility deleted *)
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

let getActionTrees trees =
    let rec aux = function
        | [] -> []
        | t :: ts -> let seeds = List.fold_left (fun acc x -> if richness x > 0 && getTreeOpt x = None then SEED(t.pos, x):: acc else acc)
                                                (aux ts) @@ getNearTiles t.pos t.size
                     in if t.size < 3 then GROW t.pos :: seeds else COMPLETE t.pos :: seeds
    in WAIT :: aux trees
and getPrice sizeArrays = function
    | WAIT -> 0
    | GROW t -> let tree = getTree t in (match tree.size with | 0 -> 1 | 1 -> 3 | 2 -> 7 |_-> failwith "illegal size") + sizeArrays.(tree.size+1)
    | SEED _ -> sizeArrays.(0)
    | COMPLETE _ -> 4
and getTarget = function
    | GROW t -> Some t
    | COMPLETE t -> Some t
    | SEED (t,_) -> Some t
    | WAIT -> None
in

let isPossible sun sizeArrays action =
    (match getTarget action with
    | None -> true
    | Some t -> not (getTree t).isdormant) && getPrice sizeArrays action <= sun
in


let richBonus t = 2*(richness t - 1) in




(* game loop *)
while true do
    let startTime = Sys.time () in
    Hashtbl.clear treeTable;
    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    let dayDebug = Printf.sprintf "day : %d/%d" day maxDay in
    prerr_endline @@ dayDebug;
    let dayf = f day in
    let nutrients = int_of_string (input_line stdin) in (* the base score you gain from the next COMPLETE action *)
    (* sun: your sun points *)
    (* score: your current score *)
    let sun, score = Scanf.sscanf (input_line stdin) " %d  %d" (fun sun score -> (sun, score)) in
    (* oppsun: opponent's sun points *)
    (* oppscore: opponent's score *)
    (* oppiswaiting: whether your opponent is asleep until the next day *)
    let oppsun, oppscore, oppiswaiting = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun oppsun oppscore oppiswaiting -> (oppsun, oppscore, oppiswaiting = 1)) in
    let numberoftrees = int_of_string (input_line stdin) and (* the current amount of trees *)
    sizeCount = Array.make 4 0
    and oppsizeCount = Array.make 4 0 in
    let acc = ref 0 in
    let rec aux i accA accE accACount = if i = numberoftrees then accA,accE, accACount else
        let tree = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d" (fun cellindex size ismine isdormant -> {pos=cellindex; size=size; ismine = (ismine = 1); isdormant = (isdormant = 1)}) in
        Hashtbl.add treeTable tree.pos tree;
        if tree.ismine then (
            sizeCount.(tree.size) <- sizeCount.(tree.size)+1;
            aux (i+1) (tree :: accA) accE (accACount+1)
        ) else (
            oppsizeCount.(tree.size) <-  oppsizeCount.(tree.size) + 1;
            aux (i+1) accA (tree :: accE) accACount
        )
    in let myTrees, oppTrees, treeCount = aux 0 [] [] 0 in
    prerr_endline "____My Trees____";
    List.iter (fun x -> prerr_endline @@ string_of_int x.pos) myTrees;
    prerr_endline "________________";
    let a x =
        Array.fold_left (+) 0 @@ Array.mapi (fun i x -> (i+1)*x) x in
    let advDebug = Printf.sprintf "%d/%d" (a sizeCount) (a oppsizeCount) in
    prerr_endline @@ "tree advantage : "^advDebug;
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let actionlist =
    let rec aux n = if n = numberofpossiblemoves then () else let _ = read_line() in aux (n+1)
    in aux 0 in
    let incomes = getIncomes sizeCount in
    let possible = isPossible sun sizeCount in
    let getScore a = (
        match a with
        | COMPLETE t -> f(richBonus t) -. 4. +.
            20. ** (((f day) /. maxDayf)**2.) +. (max 0. (f (oppscore - score -20)))
            +. if oppscore - score > -40 then f (oppsizeCount.(3)*2) else 0.
        | GROW t -> let tree = getTree t in if maxDayf -. dayf +. f tree.size < 3. then -1. else f @@ max 0 @@ 10 -
            2*sizeCount.(tree.size+1) + 2* (richBonus t)
        | WAIT -> 0.001
        | SEED (start, target) -> if maxDayf -. dayf < 3. then -1. else
        -. 0.5*. f (sizeCount.(1)) +. f ((richBonus target) - 2*sizeCount.(0))
        ) *. if (match a with | SEED (a,b) -> true | _ -> false) || possible a then 1. else 0.1
    in


    let myActions = getActionTrees myTrees in
    assert (List.fold_left (fun acc x -> acc + if possible x then 1 else 0) 0 myActions = numberofpossiblemoves);


    let sortedAction = List.sort (fun x y -> -compare (getScore x) (getScore y)) myActions in
    prerr_endline "____Actions_sorted____";
    List.iter (fun x -> prerr_endline @@ Printf.sprintf "%s : %f | %s" (toString x) (getScore x)
        (if possible x then "Doable" else "Impossible")) sortedAction;

    let comment = Printf.sprintf "time: %f " (Sys.time() -. startTime) ^ advDebug in
    match sortedAction with
    | x :: xs -> if possible x then print_endline @@ (toString x) ^" "^comment
                 else print_endline @@ "WAIT"^" WAIT FOR "^(toString x)^" "^comment
    | _ -> failwith "no action found"

    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;
