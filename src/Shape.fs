// Shape building functions that generally return/manipulate Vec2Lists
module Shape

open MathEx

let scale (x, y) verts : Vec2.List = verts |> List.map (fun (x', y') -> (x' * x, y' * y))
let scaleUni s verts : Vec2.List = scale (s, s) verts
let translate (x, y) verts : Vec2.List = verts |> List.map (fun (x', y') -> (x' + x, y' + y))
let translateInv (x, y) verts : Vec2.List = translate (-x, -y) verts

// returns (minX,minY,maxX,maxY)
let extents (v : Vec2.List) = 
    let rec f (v : Vec2.List) (minX, minY, maxX, maxY) = 
        match v with
        | [] -> (minX, minY, maxX, maxY)
        | (hx, hy) :: tail -> f tail (min hx minX, min hy minY, max hx maxX, max hy maxY)
    
    let (minX, minY), (maxX, maxY) = v.[0], v.[1]
    f v (minX, minY, maxX, maxY)

// returns center point of input extents (as Vec2)
let midpoint (v : Vec2.List) = 
    let (minX, minY, maxX, maxY) = extents v
    (minX + (maxX - minX) / 2., minY + (maxY - minY) / 2.)

// returns new Vec2.List translated so that its midpoint is (0,0) in local space
let center (v : Vec2.List) = translateInv (midpoint v) v
let shrinkToRect (minX, minY, maxX, maxY) (v : Vec2.List) = 
    v |> List.map (fun (x, y) -> x |> clamp minX maxX, y |> clamp minY maxY)

// shape functions return unit-sized shapes centered around the origin
let square = 
    [ (0., 0.)
      (0., 1.)
      (1., 1.)
      (1., 0.) ]
    |> center

let circle segs = 
    [ for s in 0..segs - 1 -> (cossin (twoPI / (float segs) * (float s))) ]

let triangle = circle 3

let line length = 
    [ (-length / 2., 0.)
      (length / 2., 0.) ]