module AstToTree

open DrawingTrees

open AST

let rec expToTree (exp : Exp) : Tree<string> =
    match exp with
    | N (n) -> Node("N", [Node(string n, [])])
    | B (b) -> Node("B", [Node(string b, [])])
    | Access (access) -> Node ("Access", [accessToTree access])
    | Addr (access) -> Node ("Addr", [accessToTree access])
    | Apply (fName, expList) -> Node ("Apply", Node (fName, []) :: List.map expToTree expList)
and accessToTree (access : Access) : Tree<string> =
    match access with
    | AVar (var) -> Node ("AVar", [Node (var, [])])
    | AIndex (acs, exp) -> Node ("AIndex", [accessToTree acs; expToTree exp] )
    | ADeref (exp) -> Node ("ADeref", [expToTree exp])
and stmtToTree (stmt : Stm) : Tree<string> =
    match stmt with
    | PrintLn (exp) -> Node ("PrintLn", [expToTree exp])
    | Ass (acs, exp) -> Node ("Ass", [accessToTree acs; expToTree exp])
    | Return (exp) ->
        Option.defaultValue (Node ("Return", [Node ("None", [])]))
                            (Option.map (fun exp' -> Node ("Return", [expToTree exp'])) exp)
    | Alt (gc) -> Node ("Alt", gcToTree gc)
    | Do (gc) -> Node ("Do", gcToTree gc)
    | Block (decList, stmtList) -> Node ("Block", List.map decToTree decList @ List.map stmtToTree stmtList)
    | Call (fName, expList) -> Node ("Call", [Node(fName, List.map expToTree expList)])

and gcToTree (gc : GuardedCommand) : Tree<string> list =
    match gc with
    | GC (gcs) -> List.map (fun (exp, stmts) -> (Node ("GC", expToTree exp :: List.map stmtToTree stmts))) gcs

and decToTree (dec : Dec) : Tree<string> =
    match dec with
    | VarDec (typ, vName) -> Node ("VarDec", typeToTree typ :: [Node (vName, [])])
    | FunDec (typOpt, fName, decs, stmt) ->
        let nodeType = Option.defaultValue (Node("NoType", [])) (Option.map typeToTree typOpt)
        let convertDecs = List.map decToTree decs
        Node ("FunDec", [nodeType; Node(fName, [])] @ convertDecs @ [stmtToTree stmt] )
and typeToTree (typ : Typ) : Tree<string> =
    match typ with
    | ITyp -> Node ("ITyp", [])
    | BTyp -> Node ("BTyp", [])
    | ATyp (t, x) ->
        Node ("ATyp", typeToTree t :: [Node (Option.defaultValue "NaN" (Option.map string x), [])])
    | PTyp t -> Node ("PTyp", [typeToTree t])
    | FTyp (typs, typOpt) -> Node ("FTyp", List.map typeToTree typs @ [Option.defaultValue (Node("NoType", [])) (Option.map typeToTree typOpt)])

let toGeneralTree (P(decs, stmts) : Program) : Tree<string> =
    Node("Program", List.concat [List.map decToTree decs; List.map stmtToTree stmts])
