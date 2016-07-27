module Vec2

open System
open MathEx

type T = float * float

type List = T list

let zero = 0., 0.
let add (x1, y1) (x2, y2) : T = x1 + x2, y1 + y2
let sub (x1, y1) (x2, y2) : T = x1 - x2, y1 - y2
let elementWiseMul (x1, y1) (x2, y2) : T = x1 * x2, y1 * y2
let mul s (x, y) : T = x * s, y * s
let addMul (x, y) (dx, dy) s : T = x + dx * s, y + dy * s
let magnitude (x, y) = Math.Sqrt(x * x + y * y)

let normalize (x, y) = 
    let mag = magnitude (x, y)
    assert (mag <> 0.)
    x / mag, y / mag

let fromAngle rads = cossin (rads)

let toAngle (x, y) = 
    let (x, y) = normalize (x, y)
    atan2 (y, x)

let dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
let cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2
let distance v1 v2 = magnitude (sub v1 v2)
// returns rads in [0,PI]
let shortestAngleDiff v1 v2 = Math.Acos(dot (normalize (v1)) (normalize (v2)))
// returns angle in [-PI,PI]
let signedAngleDiff v1 v2 = Math.Atan2(cross v1 v2, dot v1 v2)