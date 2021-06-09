module DrawingTreesPropTests

open FsCheck


open DrawingTrees

let rec distanced list =
    match list with
    |a::b::t when (b-a>=1.0) -> distanced (b::t)
    |a::b::t -> false
    |_ -> true

let absolutePosTree (posTree : Tree<'a * float>) : Tree<'a * float> =
    let rec absDistance parentPos tree =
        match tree with
        | Node((a, pos), subtree) -> Node((a, pos + parentPos), List.map (fun tree -> absDistance (pos + parentPos) tree) subtree)
    absDistance 0.0 posTree

let posByLayer posTree =
    let rec bfs trees positions queue =
        match trees, queue with 
        | Node((_,pos),subtrees)::t, _ -> bfs t (pos::positions) (queue @ subtrees)
        | _, [] -> [positions |> List.rev]
        | _,_  -> (positions |> List.rev) :: (bfs queue [] [])
    bfs [posTree] [] []

let rec nodeDistanceProp tree =
    let rec sortedPosList posList =
        match posList with
            |h::t when distanced h -> sortedPosList t
            |h::t -> false
            |_ -> true
    sortedPosList(posByLayer(absolutePosTree(tree)))

let centered (Node (a,pos),subtree) = 
    match subtree.Head, (subtree |> List.rev).Head with
    | a,b when (b-a)/2.0 = pos -> true
    | _ -> false

let rec parentCenteredProp trees =
     match trees with
     |( Node((a,pos),subtrees))::t when centered h -> parentCenteredProp t && parentCenteredProp subtrees
     | Node(_) -> false
     |_ -> true