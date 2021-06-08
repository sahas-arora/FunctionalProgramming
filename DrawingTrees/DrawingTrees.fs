module DrawingTrees

type Tree<'a> = Node of ('a * Tree<'a> list)

type Extent = (float * float) list

let moveTree (Node ((label, x), subTrees)) (x': float) = Node((label, x + x'), subTrees)

let moveExtent (e: Extent) (x: float) =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec merge e1 e2 =
    match e1, e2 with
    | [], qs -> qs
    | ps, [] -> ps
    | (p, _) :: ps, (_, q) :: qs -> (p, q) :: (merge ps qs)

let rec mergeList (es: Extent list) : Extent =
    List.fold (fun acc (e: Extent) -> merge e acc) [] es

let rmax (p: float) (q: float) : float = if p > q then p else q

let rec fit e1 e2 =
    match (e1, e2) with
    | ((_, p) :: ps), ((q, _) :: qs) -> rmax (fit ps qs) (p - q + 1.0)
    | _ -> 0.0


let fitListl (es: Extent list) =
    let rec fitListl' acc es =
        match es with
        | [] -> []
        | (e :: es') -> let x = fit acc e in x :: fitListl' (merge acc (moveExtent e x)) es'

    fitListl' [] es


let fitListr (es: Extent list) =
    let rec fitListr' acc es =
        match es with
        | [] -> []
        | (e :: es') -> let x = -(fit e acc) in x :: fitListr' (merge (moveExtent e x) acc) es'

    fitListr' [] (List.rev es) |> List.rev


let mean x y = (x + y) / 2.0

let fitList (es: Extent list) =
    List.map2 mean (fitListl es) (fitListr es)

let design (tree: Tree<'a>) : Tree<'a * float> =
    let rec design' (Node (label, subTrees)) =
        let (trees, extents) = List.unzip (List.map design' subTrees)
        let positions = fitList extents
        let ptrees = List.map2 moveTree trees positions
        let pextents = List.map2 moveExtent extents positions
        let resultextent = (0.0, 0.0) :: mergeList pextents
        let resultTree = Node((label, 0.0), ptrees)
        (resultTree, resultextent)

    fst (design' tree)


let bfs tree =
    let rec bfs trees positions queue =
        match trees, queue with 
        | Node((_,pos),subtrees)::t, _ -> bfs t (pos::positions) (queue @ subtrees)
        | _, [] -> []
        | _,_  -> (positions |> List.rev) :: bfs queue [] []
    bfs [tree] [] []