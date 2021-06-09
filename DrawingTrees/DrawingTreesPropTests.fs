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


let headAndTailPos subtrees =
    match subtrees with 
    |Node((a,pos1),trees)::t -> match subtrees |> List.rev with
                                |Node((a,pos2),trees)::_ -> pos1,pos2
                                | _-> 0.0,0.0
    |_ -> 0.0,0.0

let centered subtrees = 
    match (headAndTailPos (subtrees)) with
    |(a,b) when a+b=0.0 -> true          
    | _ -> false

let centProp tree =
    let rec parentCenteredProp trees =
         match trees with
         |( Node((a,pos),subtrees))::t when centered (subtrees) -> parentCenteredProp subtrees && parentCenteredProp t  
         | [] -> true
         | _ -> false
    parentCenteredProp [tree]

// check if l1 equals l2 with inverted signs
let rec reflectedList l1 l2 =
    match l1, l2 with 
    | h1::t1, h2::t2 when h1 = -h2 -> reflectedList t1 t2
    | [], [] -> truee 
    | _ -> false

// check if reflected tree is equal to original tree with horizontal position and sign inverted
let reflectProp tree =
    let rec reflectProp treeLayers reflTreeLayers =
        match treeLayers, reflTreeLayers with
        | h1::t1, h2::t2 when reflectedList (h1 |> List.rev) h2 -> reflectProp t1 t2   
    reflectProp (posByLayer(tree)) (posByLayer(reflect(tree)))  




//let tree = Node(("a",0.0),[Node(("b",-0.5),[]);Node(("c",1.5),[])])

//let subtrees =[Node(("b",-0.5),[]);Node(("c",1.5),[])]