#if !INTERACTIVE
module Main
open OpenTKPlatform
#endif

//#if INTERACTIVE
//#load "OpenTKPlatform.fs"
//#endif

open System

let swapArgs f a b = f b a

type Vec = (float*float)
type VecList = Vec list

type Visual = {
    vertices:VecList list
}

type Actor = {
    pos:Vec
    angle:float
    speed:float
    visual:Visual
}

let defaultActor = {
    pos = (0.,0.)
    angle = 0.
    speed=0.
    visual = { vertices = [[]] }
}

let cos = Math.Cos
let sin = Math.Sin
let cossin rads :Vec = (cos(rads), sin(rads))

let drawVisual (canvas:Canvas) (vis:Visual) =
    vis.vertices |> List.iter canvas.drawVertices

let drawActor (canvas:Canvas) (actor:Actor) =
    canvas.save ()
    canvas.translate actor.pos
    canvas.rotate (degToRad actor.angle)
    drawVisual canvas actor.visual
    canvas.restore ()

// Returns a single list of all elements in a list of lists
// e.g. [[1;2]; [3;4]] -> [1;2;3;4]
let flattenListList (x:'a list list) = x |> List.reduce List.append

module ShapeBuilder =
    let scale (x,y) verts = verts |> List.map(fun (x',y') -> (x'*x, y'*y))
    let scaleUni s verts = scale (s,s) verts
    let translate (x,y) verts = verts |> List.map(fun (x',y') -> (x'+x, y'+y))
    let translateInv (x,y) verts = translate (-x,-y) verts

    // returns (minX,minY,maxX,maxY)
    let extents (v:VecList) =
        let rec f (v:VecList) (minX,minY,maxX,maxY) =
            match v with
            | [] -> (minX,minY,maxX,maxY)
            | (hx,hy) :: tail -> f tail (Math.Min(hx,minX), Math.Min(hy,minY), Math.Max(hx,maxX), Math.Max(hy,maxY))
        f v (0.,0.,0.,0.)

    // returns center point of input extents (as Vec)
    let midpoint (v:VecList) =
        let (minX,minY,maxX,maxY) = extents v
        (minX + (maxX-minX)/2., minY + (maxY-minY)/2.)

    // returns new VecList translated so that its midpoint is (0,0) in local space
    let center (v:VecList) = translateInv (midpoint v) v
    
    // shape functions return unit-sized shapes centered around the origin
    let square = [ (0.,0.); (0.,1.); (1.,1.); (1.,0.) ] |> center
    let circle segs = [ for s in 0..segs -> (cossin(2. * Math.PI/(float segs) * (float s))) ]
    let triangle = circle 3


open ShapeBuilder

let makeShipVisual () =
    let body = square |> scale (28., 16.)
    let turret = triangle |> scale (20., 4.) |> translate (5., 0.)
    let buildWheels size num =
        [for i in 0. .. (float)num-1. do yield! translate (i*size, 0.) (scaleUni size square)] |> center
    let wheels = buildWheels 6. 5
    let wheel1 = wheels |> translate (0., -13.)
    let wheel2 = wheels |> translate (0., 13.)
    { vertices = [body; wheel1; wheel2; turret] }


let add ((x1,y1):Vec) ((x2,y2):Vec) = (x1 + x2, y1 + y2)
let addMul (x,y) (dx,dy) s = (x + dx * s, y + dy * s)
let angleToVector degs = cossin(degToRad degs)
let rotateActor degs actor = {actor with angle = actor.angle + degs}

let rec update (app:Application) (actors:Actor list) (dt:float) (canvas:Canvas) =
    // Logic update
    let player = actors.[0]
    
    let turnRate = 360.
    let angleDelta =
        if Keyboard.IsDown Key.Left then turnRate
        elif Keyboard.IsDown Key.Right then -turnRate
        else 0.
    let finalAngle = player.angle + angleDelta * dt

    let speedUpRate = 200.
    let speedDownRate = -800.
    let autoSpeedDownRate = -200.
    let maxSpeed = 400.
    let speedDelta =
        if Keyboard.IsDown Key.Up then speedUpRate
        elif Keyboard.IsDown Key.Down then speedDownRate
        else autoSpeedDownRate
    let finalSpeed = Math.Min(Math.Max(0., player.speed + speedDelta * dt), maxSpeed)

    let moveDelta = finalSpeed * dt
    let forward = angleToVector finalAngle
    let finalPos = addMul player.pos forward moveDelta

    let player = {
        player with
            angle = finalAngle
            pos = finalPos
            speed = finalSpeed
        }

    let others = List.tail actors |> List.map (rotateActor (100. * dt))
    let actors = player :: others

    // To keep it pure, we need to re-register a new instance of update that
    // binds the udpated actors list
    app.setOnUpdate (update app actors)

    // Render
    canvas.resetTransform ()
    canvas.clear ()
    actors |> List.iter (fun (actor) -> drawActor canvas actor)


let main () =
    let ship = {
        defaultActor with 
            pos = (100.,100.)
            angle = 180.
            visual = makeShipVisual ()
    }

    let actors = [
        {ship with pos=(50.,50.); angle=20.}
        {ship with pos=(150.,150.); angle=45.}
        {ship with pos=(250.,250.); angle=95.}
    ]
    
    let app = new Application("FSharpRipOff", (800, 600))
    app.setOnUpdate (update app actors)
    app.run ()

main ()
