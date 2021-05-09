(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
(* TODO get shadow diff for complete/grow tree *)
(* TODO optimize plants at the end, we need less seeds and more trees *)
(* TODO add actions of dormant trees to the list *)
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


(* game loop *)
while true do
    let treeTable = Hashtbl.create 37 in
    let getTreeOpt x = Hashtbl.find_opt treeTable x and
    getTree x = Hashtbl.find treeTable x in
    let getNearTrees pos =
        let queue = Queue.create() in
        Queue.add (pos, 0) queue;
        let rec aux ignore =
            if Queue.is_empty queue then [] else
            let pos, depth = Queue.take queue in
            if depth > 3 then [] else
            let l = aux @@ IntSet.union (Array.fold_left (fun acc n -> if n >= 0 && not (IntSet.mem n ignore) then
                                     (Queue.add (n,depth+1) queue; IntSet.add n acc)
                                     else acc) IntSet.empty @@ getNeighbours pos)
                                     (IntSet.add pos ignore) in
            match getTreeOpt pos with
            | None -> l
            | Some x -> x::l
        in aux IntSet.empty
    in
    let visibility tree =
        let rec aux pos acc dir = if acc>3 || pos = -1 then 0
            else ((match getTreeOpt pos with
                | None -> 0
                | Some x -> tree.size - 2*x.size - 6 + acc)
                + aux neighbourTable.(pos).(dir) (acc+1) dir
            )
        and loop i acc = if i = 6 then acc else loop (i+1) (acc + aux tree.pos 0 i)
        in loop 0 0
    in


    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    prerr_endline @@ Printf.sprintf "day : %d/%d" day maxDay;
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
    for i = 0 to numberoftrees - 1 do
        (* cellindex: location of this tree *)
        (* size: size of this tree: 0-3 *)
        (* ismine: 1 if this is your tree *)
        (* isdormant: 1 if this tree is dormant *)
        let tree = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d" (fun cellindex size ismine isdormant -> {pos=cellindex; size=size; ismine = (ismine = 1); isdormant = (isdormant = 1)}) in
        Hashtbl.add treeTable tree.pos tree;
        if tree.ismine then (
            sizeCount.(tree.size) <- sizeCount.(tree.size)+1;
            incr acc
        ) else (
            oppsizeCount.(tree.size) <-  oppsizeCount.(tree.size) + 1
        )
    done;
     let a x =
        Array.fold_left (+) 0 @@ Array.mapi (fun i x -> (i+1)*x) x in
    prerr_endline @@ Printf.sprintf "tree advantage : %d/%d"
        (a sizeCount) (a oppsizeCount);

    let treeCount = !acc in
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let actionlist =
    let rec aux n = if n=numberofpossiblemoves then [] else (action (input_line stdin) :: aux (n+1))
    in aux 0 in
    let incomes = getIncomes sizeCount in
    let getScore = function
        | COMPLETE t -> f(richness t (* - (visibility (getTree t))/5 *) ) -. 1. +.
            20. ** (((f day) /. maxDayf)**2.) +. (max 0. (f (oppscore - score -20)))
            +. if oppscore - score > -40 then f (oppsizeCount.(3)*2) else 0.
        | GROW t -> let tree = getTree t in if maxDayf -. dayf +. f tree.size < 3. then -1. else f @@ max 0 @@ 10 - 2*sizeCount.(tree.size+1) + 2* (richness t)
        | WAIT -> 0.001
        | SEED (start, target) -> if maxDayf -. dayf < 3. then -1. else
        -. 0.5*. f (sizeCount.(1)) +.
        (f @@ (richness target) - 2*sizeCount.(0)  + 0*(List.fold_left
            (fun acc t -> acc + if t.ismine then -1 else 0) 0 (getNearTrees target)))
            (* +let v = visibility (makeSeed target) in if v=0 then 100 else v *)

        in
    let sortedAction = List.sort (fun x y -> -compare (getScore x) (getScore y)) actionlist in
    List.iter (fun x -> prerr_endline @@ Printf.sprintf "%s : %f" (toString x) (getScore x)) sortedAction;
    match sortedAction with
    | x :: xs -> print_endline @@ toString x
    | _ -> failwith "no action found"

    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;
