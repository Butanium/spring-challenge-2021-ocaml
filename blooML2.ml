
(* TODO adjust endGame, we could optimize it *)


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



let maxDayf = 23. and
maxDay = 23 in
let treeTable = Hashtbl.create 37 in
let getTreeOpt x = Hashtbl.find_opt treeTable x and
    getTree x = Hashtbl.find treeTable x in

(* getNearTrees deleted *)
(* getVisibility deleted *)

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

let getSelfShadowSize tree treeSize =
    let rec aux (pos:int) dir depth ((maxHeight,hPos),acc) =
        let n =  (getNeighbours pos).(dir) in
        if n = -1 || depth > treeSize then acc else (
            aux n dir (depth + 1) @@
            match getTreeOpt n with
            (* if t.size < depth then less malus *)
            | Some t -> (if t.size >= maxHeight then t.size, depth else maxHeight, hPos),
                if t.size > maxHeight || depth - hPos > maxHeight then acc + (if t.ismine then t.size else -t.size)
                else acc
            | _ -> (maxHeight,hPos),acc
        )
    and loop i acc = if i = 6 then acc else
        loop (i+1) (acc + aux tree i 1 ((0,0),0))
    in f @@ loop 0 0 in
let getSelfShadow tree =
    getSelfShadowSize tree (match getTreeOpt tree with | None -> -1 | Some t -> t.size)  in

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

(* game loop *)
while true do
    let startTime = Sys.time () in
    Hashtbl.clear treeTable;
    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    let dayDebug = Printf.sprintf "day : %d/%d" day maxDay in
    prerr_endline @@ dayDebug;
    let dayf = f day in
    let nutrients = int_of_string (input_line stdin) in (* the base score you gain from the next COMPLETE action *)

    let sun, score = Scanf.sscanf (input_line stdin) " %d  %d" (fun sun score -> (sun, score)) in

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
    let enTreeRatio = f (a oppsizeCount) /. f (a sizeCount) in
    prerr_endline @@ "tree advantage : "^advDebug;
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let actionlist =
    let rec aux n = if n = numberofpossiblemoves then () else let _ = read_line() in aux (n+1)
    in aux 0 in
    let possible = isPossible sun sizeCount in
    let getScore a = let s1,s2 = (
        match a with
        | COMPLETE t -> let nb3 = sizeCount.(3) in -. 4. +. 20. ** ((dayf/. maxDayf)**2.) +.
            (if nb3>2 then f nb3 else 0.)+. (if day=maxDay then 10. else (min 0. (4. *.(1. -. enTreeRatio))))
            +. 2. +.  getDiffF (getTree t) (makeSeed t) 2 (min 2 (maxDay-day)) +. max 0. (f sun -. 5.),
            f (richBonus t)
        | SEED (x,y) -> if maxDay - day < 3 then -1. , 0. else (if day <= 1 then 0. else 10.) *. (exp (-.2.4 *.(f sizeCount.(0))**2.))
        , 0.25 *. f(richBonus y) +. 2. *.getPotentialAllyShadow y
        | GROW t ->let tree = getTree t in let size = tree.size in if maxDay - day + size < 3 then -1. , 0. else 5. +. 2. *.(f size -. 1.5)*.dayf/.maxDayf -. f sizeCount.(size+1)
         +. 2. +. getDiffF tree ({pos=t; size=size + 1; ismine=true; isdormant=true}) (min 2 (maxDay-day)) day , f (richBonus t)
        | WAIT -> 0., 0.

        ) in s1 *. (if (match a with | SEED _ -> true | _ -> false) || possible a then 1. else 0.5),
         if (match a with SEED _ -> true | _ ->false) && not @@ possible a then 0.5 *. s2 else s2
    in

    let myActions = getActionTrees myTrees in
    assert (List.fold_left (fun acc x -> acc + if possible x then 1 else 0) 0 myActions = numberofpossiblemoves);

    let sortedAction = List.sort (fun x y ->
        let (xs1, xs2), (ys1, ys2) = (getScore x), (getScore y) in -2*compare xs1 ys1 - compare xs2 ys2) myActions in
    prerr_endline "____Actions_sorted____";
    List.iter (fun x -> let s1,s2 = getScore x in  prerr_endline @@ Printf.sprintf "%s : %f, %f | %s" (toString x) s1 s2
        (if possible x then "Y" else "N")) sortedAction;

    let comment = Printf.sprintf "time: %f " (Sys.time() -. startTime) ^ advDebug in
    match sortedAction with
    | x :: xs -> if possible x then print_endline @@ (toString x) ^" "^comment
                 else print_endline @@ "WAIT"^" WAIT FOR "^(toString x)^" "^comment
    | _ -> failwith "no action found"

    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;