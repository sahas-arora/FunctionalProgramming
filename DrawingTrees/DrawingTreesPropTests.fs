module DrawingTreesPropTests

open FsCheck

open DrawingTrees

//let nodeDistanceProp tree =




(* let parentCenteredProp tree =
     match subtree with
         | Node((a, pos), subtree) -> match subtree.Head, (subtree |> List.rev).Head with


                let rec centered subtree =

                    |Node((_,pos1),subtree1), Node((_,pos2),subtree2) when pos1-pos2=0 -> (centered subtree1) && (centered subtree2)

                    |_ -> false
    centered tree *)