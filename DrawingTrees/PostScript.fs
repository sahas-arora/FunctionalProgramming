module PostScript

open DrawingTrees

let toPS (body : string) : string = "%!
<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice
1 1 scale
700 999 translate
newpath
/Times-Roman findfont 10 scalefont setfont"
+
body
+
"stroke\n showpage"


let toPSslow (Node((label, pos), subtree) : Tree<string * float>) : string =
    failwith "not implemented"


let toPSfast (tree : Tree<string * float>) : string =
    failwith "not implemented"



let treeToFile (psString : string) (tree : Tree<string>) : unit =
    failwith "not implemented"


let posTreeToFile (psString : string) (ptree : Tree<string * float>) : unit =
    failwith "not implemented"
