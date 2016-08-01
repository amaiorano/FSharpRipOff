module Visual

type T = 
    { // List of vertex lists, each of which will be drawn as a line loop
      vertLists : Vec2.List list
      colors : Color.T list }

let defaultVisual = 
    { vertLists = [ [] ]
      colors = [] }

// Breaks apart each vert list into list of lines
let breakIntoLines visual = 
    let folder (newVertLists, newColors) oldVerts oldColor = 
        let lines = 
            oldVerts @ [ oldVerts.[0] ] // Append initial element at the back since we expect a line loop
            |> Seq.pairwise
            |> Seq.map (fun (x, y) -> [ x; y ])
        (Seq.append lines newVertLists, Seq.append (Seq.replicate (Seq.length lines) oldColor) newColors)
    
    let vertLists, colors = (visual.vertLists, visual.colors) ||> Seq.fold2 folder (Seq.empty, Seq.empty)
    { visual with vertLists = List.ofSeq vertLists
                  colors = List.ofSeq colors }

let draw (canvas : OpenTKPlatform.Canvas) (vis : T) = 
    List.iter2 (fun verts color -> canvas.drawVerticesColors verts color) vis.vertLists vis.colors