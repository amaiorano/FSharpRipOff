module Visual

type T = 
    { // List of vertex lists, each of which will be drawn as a line loop
      vertLists : Vec2.List list }

// Breaks apart each vert list into list of lines
let breakIntoLines visual = 
    //@TODO: utility module
    let removeOuterList (list : 'a list list) = 
        [ for x in list do
              yield! x ]
    
    let vertLists = 
        visual.vertLists
        |> List.map (fun verts -> 
               verts @ [ verts.[0] ] //@TODO: inefficient!
               |> List.pairwise
               |> List.map (fun (x, y) -> [ x; y ]))
        |> removeOuterList
    
    { visual with vertLists = vertLists }
