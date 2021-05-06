(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
type tree = {pos : int; size : int; ismine : bool; isdormant : bool};;
type action = GROW of int | COMPLETE of int | WAIT;;
let action s = if s.[0] = 'W' then WAIT else (
    let act, t = Scanf.sscanf s "%s %d" (fun a b -> a,b) in
    match act.[0] with
    | 'G' -> GROW t
    | 'C' -> COMPLETE t
    | _ -> failwith "not implemented"
    )

and toString = function
    | GROW t -> Printf.sprintf "GROW %d" t
    | COMPLETE t -> Printf.sprintf "COMPLETE %d" t
    | WAIT -> "WAIT" in

let neighbourTable = Hashtbl.create 37
and richnessTable = Hashtbl.create 37 in
let getNeighbours x = Hashtbl.find neighbourTable x
and richness x = Hashtbl.find richnessTable x  in

let numberofcells = int_of_string (input_line stdin) in (* 37 *)
for i = 0 to numberofcells - 1 do
    (* index: 0 is the center cell, the next cells spiral outwards *)
    (* richness: 0 if the cell is unusable, 1-3 for usable cells *)
    (* neigh0: the index of the neighbouring cell for each direction *)
    let index, richness, neigh0, neigh1, neigh2, neigh3, neigh4, neigh5 = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d  %d  %d" (fun index richness neigh0 neigh1 neigh2 neigh3 neigh4 neigh5 -> (index, richness, neigh0, neigh1, neigh2, neigh3, neigh4, neigh5)) in
    Hashtbl.add richnessTable index richness ;
    Hashtbl.add neighbourTable index [neigh0; neigh1; neigh2; neigh3; neigh4; neigh5];
done;
let getIncomes trees = Array.fold_left (+) 0 @@ Array.mapi (fun i x -> x*i) trees in


let maxDay = 5 in

(* game loop *)
while true do
    let treeTable = Hashtbl.create 37 in
    let getTree x = Hashtbl.find treeTable x in
    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    let nutrients = int_of_string (input_line stdin) in (* the base score you gain from the next COMPLETE action *)
    (* sun: your sun points *)
    (* score: your current score *)
    let sun, score = Scanf.sscanf (input_line stdin) " %d  %d" (fun sun score -> (sun, score)) in
    (* oppsun: opponent's sun points *)
    (* oppscore: opponent's score *)
    (* oppiswaiting: whether your opponent is asleep until the next day *)
    let oppsun, oppscore, oppiswaiting = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun oppsun oppscore oppiswaiting -> (oppsun, oppscore, oppiswaiting = 1)) in
    let numberoftrees = int_of_string (input_line stdin) and (* the current amount of trees *)
    sizeCount = Array.make 4 0 in
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
        )
    done;
    let treeCount = !acc in
    let numberofpossiblemoves = int_of_string (input_line stdin) in
    let actionlist =
    let rec aux n = if n=numberofpossiblemoves then [] else (action (input_line stdin) :: aux (n+1))
    in aux 0 in
    let incomes = getIncomes sizeCount in
    let getScore = function
        | COMPLETE t -> (sun + incomes)/3 + richness t
        | GROW t -> max 0 @@ 10 - 2*sizeCount.((getTree t).size+1) + richness t
        | WAIT -> -100 in
    let sortedAction = List.sort (fun x y -> -compare (getScore x) (getScore y)) actionlist in
    match sortedAction with
    | x :: xs -> print_endline @@ toString x
    | _ -> failwith "no action found"

    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)

done;;
