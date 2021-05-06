(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
type tree = {pos : int; size : int; ismine : bool; isdormant : bool};;
type action = GROW of int | COMPLETE of int | WAIT;;
let action s = if s.[0] = 'W' then WAIT else (
    let act, t = Scanf.sscanf (input_line stdin) "%s %d" (fun a b -> a,b) in
    match act.[0] with
    | 'G' -> GROW t
    | 'C' -> COMPLETE t
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


(* game loop *)
while true do
    let treeTable = Hashtbl.create 37 in
    let day = int_of_string (input_line stdin) in (* the game lasts 24 days: 0-23 *)
    let nutrients = int_of_string (input_line stdin) in (* the base score you gain from the next COMPLETE action *)
    (* sun: your sun points *)
    (* score: your current score *)
    let sun, score = Scanf.sscanf (input_line stdin) " %d  %d" (fun sun score -> (sun, score)) in
    (* oppsun: opponent's sun points *)
    (* oppscore: opponent's score *)
    (* oppiswaiting: whether your opponent is asleep until the next day *)
    let oppsun, oppscore, oppiswaiting = Scanf.sscanf (input_line stdin) " %d  %d  %d" (fun oppsun oppscore oppiswaiting -> (oppsun, oppscore, oppiswaiting = 1)) in
    let numberoftrees = int_of_string (input_line stdin) in (* the current amount of trees *)
    for i = 0 to numberoftrees - 1 do
        (* cellindex: location of this tree *)
        (* size: size of this tree: 0-3 *)
        (* ismine: 1 if this is your tree *)
        (* isdormant: 1 if this tree is dormant *)
        let tree = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d" (fun cellindex size ismine isdormant -> {pos=cellindex; size=size; ismine = (ismine = 1); isdormant = (isdormant = 1)}) in
        ();
    done;

    let numberofpossiblemoves = int_of_string (input_line stdin) in
    for i = 0 to numberofpossiblemoves - 1 do
        let possiblemove = input_line stdin in
        ();
    done;


    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message> *)
    print_endline "WAIT";
    ();
done;
