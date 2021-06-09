open DrawingTrees


let randomSquareTree width depth =
    let rand = System.Random()
    let randomizedMatrix =
        [1 .. depth]
        |> List.map (fun _ ->
            [1 .. width] |> List.map (fun _ -> rand.Next(0,width))
        )
    let nodes =
        randomizedMatrix
        |> List.fold (fun s xs ->
            [0 .. width-1] |> List.map (fun i -> Node("s", (List.zip s xs) |> (List.filter (fun (_, j) -> j = i )) |> (List.map (fst)) ))
        ) ([1 .. width] |> List.map (fun _ -> Node("l", [])))
    Node("r", nodes)


let tree = randomSquareTree 5 5;;

[<EntryPoint>]
let main argv =
    let myTree = Node ("A",
                    [Node ("B",
                        [Node ("C",
                            [Node ("D", [])])])])
    let exTree = randomSquareTree 5 2
    printf "%A" (design exTree)

    0