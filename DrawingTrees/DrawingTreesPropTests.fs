module DrawingTreesPropTests

open FsCheck


open DrawingTrees



let absolutePosTree (posTree : Tree<'a * float>) : Tree<'a * float> =
    let rec absDistance parentPos tree =
        match tree with
        | Node((a, pos), subtree) -> Node((a, pos + parentPos), List.map (fun tree -> absDistance (pos + parentPos) tree) subtree)

    absDistance 0.0 posTree





let nodeDistanceProp tree =
    let rec offspringDistance subtree =
        match subtree with
            | Node((a1,pos1),subtree1) ::  Node((a2,pos2),subtree2) :: t when pos2-pos1>=1.0 -> offspringDistance (Node((a2,pos2),subtree2) :: t)
            | n1 :: n2 :: t -> false
            | _ -> true

    let rec nodeDistanceProp tree =
        match tree with
            | Node((a,pos), subtree) :: t -> offspringDistance subtree && nodeDistanceProp subtree
                                             && nodeDistanceProp t
            | _ -> true

    nodeDistanceProp [tree]


(* let parentCenteredProp tree =
     match subtree with
         | Node((a, pos), subtree) -> match subtree.Head, (subtree |> List.rev).Head with


                let rec centered subtree =

                    |Node((_,pos1),subtree1), Node((_,pos2),subtree2) when pos1-pos2=0 -> (centered subtree1) && (centered subtree2)

                    |_ -> false
    centered tree *)