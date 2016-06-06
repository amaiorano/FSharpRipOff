﻿#if !INTERACTIVE
module Main
open OpenTKPlatform
#endif

//#if INTERACTIVE
//#load "OpenTKPlatform.fs"
//#endif

open System

let swapArgs f a b = f b a

let cos = Math.Cos
let sin = Math.Sin
let cossin rads = (cos(rads), sin(rads))
let twoPI = 2. * Math.PI


module Vec2 =
    type T = (float * float)
    let add (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
    let mul s (x,y) = (x * s, y * s)
    let addMul (x,y) (dx,dy) s = (x + dx * s, y + dy * s)    
    let fromAngle degs = cossin(degToRad degs)
    let magnitude (x,y) = Math.Sqrt(x*x + y*y)
    let normalize (x,y) =
        let mag = magnitude (x,y)
        assert (mag <> 0.)
        (x / mag, y / mag)
    let dot (x1,y1) (x2,y2) = x1*x2 + y1*y2
    // returns rads
    let angleDiff v1 v2 = Math.Acos (dot (normalize(v1)) (normalize(v2)))

type Vec2List = Vec2.T list

type Visual = {
    vertices:Vec2List list
}

type Actor = {
    pos:Vec2.T
    angle:float
    speed:float
    visual:Visual
}
let defaultActor = {
    pos = (0.,0.)
    angle = 0.
    speed = 0.
    visual = { vertices = [[]] }
}

type EnemyState =
    | Idle
    | GrabBarrel of barrel:Actor
    | Leave
    | AttackPlayer of player:Actor

type Enemy = {
    actor : Actor
    state : EnemyState
}
let defaultEnemy = {
    actor = defaultActor
    state = Idle
}

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
    let extents (v:Vec2List) =
        let rec f (v:Vec2List) (minX,minY,maxX,maxY) =
            match v with
            | [] -> (minX,minY,maxX,maxY)
            | (hx,hy) :: tail -> f tail (Math.Min(hx,minX), Math.Min(hy,minY), Math.Max(hx,maxX), Math.Max(hy,maxY))
        f v (0.,0.,0.,0.)

    // returns center point of input extents (as Vec2)
    let midpoint (v:Vec2List) =
        let (minX,minY,maxX,maxY) = extents v
        (minX + (maxX-minX)/2., minY + (maxY-minY)/2.)

    // returns new Vec2List translated so that its midpoint is (0,0) in local space
    let center (v:Vec2List) = translateInv (midpoint v) v
    
    // shape functions return unit-sized shapes centered around the origin
    let square = [ (0.,0.); (0.,1.); (1.,1.); (1.,0.) ] |> center
    let circle segs = [ for s in 0..segs -> (cossin(twoPI/(float segs) * (float s))) ]
    let triangle = circle 3


module GameVisual =
    open ShapeBuilder

    let makeTank () =
        let body = square |> scale (28., 16.)
        let turret = triangle |> scale (20., 4.) |> translate (5., 0.)
        let buildWheels size num =
            [for i in 0. .. (float)num-1. do 
                yield! translate (i*size, 0.) (scaleUni size square)] |> center
        let wheels = buildWheels 6. 5
        let wheel1 = wheels |> translate (0., -13.)
        let wheel2 = wheels |> translate (0., 13.)
        { vertices = [body; wheel1; wheel2; turret] }

    let makeEnemy () =
        let body = square |> scale (25., 16.)
        let body2 = circle 8 |> scaleUni 5.
        let turret = triangle |> scale (10., 4.) |> translate (12., 0.)
        let turret1 = turret |> translate(0., -5.)
        let turret2 = turret |> translate(0., 5.)
        { vertices = [body; body2; turret1; turret2] }

    let makeBarrel () =
        { vertices = [circle 6 |> scaleUni 10.] }

//let rotateActor degs actor = {actor with angle = actor.angle + degs}

type GameData = {
    player:Actor;
    barrels:Actor list;
    enemies:Enemy list;
}

let allActors gameData = [gameData.player] @ gameData.barrels @ (gameData.enemies |> List.map (fun e -> e.actor))

let movePlayer dt player =
    let turnRate = 360.
    let angleDelta =
        if Keyboard.IsDown Key.Left then turnRate
        elif Keyboard.IsDown Key.Right then -turnRate
        else 0.
    let finalAngle = player.angle + angleDelta * dt

    let speedUpRate = 300.
    let speedDownRate = -800.
    let autoSpeedDownRate = -300.
    let maxSpeed = 200.
    let speedDelta =
        if Keyboard.IsDown Key.Up then speedUpRate
        elif Keyboard.IsDown Key.Down then speedDownRate
        else autoSpeedDownRate
    let finalSpeed = Math.Min(Math.Max(0., player.speed + speedDelta * dt), maxSpeed)

    let moveDelta = finalSpeed * dt
    let forward = Vec2.fromAngle finalAngle
    let finalPos = Vec2.addMul player.pos forward moveDelta
    { player with pos = finalPos; angle = finalAngle; speed = finalSpeed }    

let updateEnemy dt (gameData:GameData) (enemy:Enemy) =
    let enemy =
        match enemy.state with
        | Idle -> {enemy with state = GrabBarrel(gameData.barrels.[0])}
        | GrabBarrel(barrel) ->
            // Turn and move towards the barrel
            // If we're close enough, change state to Leave
            enemy
        | Leave -> enemy
        | AttackPlayer(player) -> enemy

    enemy

let rec update (app:Application) (gameData:GameData) (dt:float) (canvas:Canvas) =
    // Logic update
    let player = gameData.player |> movePlayer dt
    let enemies = gameData.enemies |> List.map (updateEnemy dt gameData)

    // To keep it pure, we need to re-register a new instance of update that
    // binds the updated gameData
    let gameData = { gameData with player = player; enemies = enemies }
    app.setOnUpdate (update app gameData)

    // Render
    canvas.resetTransform ()
    canvas.clear ()
    allActors gameData |> List.iter (fun (actor) -> drawActor canvas actor)

let main () =
    let player = { defaultActor with pos = (120.,40.); angle = 120.; visual = GameVisual.makeTank() }
    let barrel = { defaultActor with visual = GameVisual.makeBarrel() }
    let enemy = { defaultEnemy with actor = { defaultActor with pos = (-200.0, 200.0); visual = GameVisual.makeEnemy() } }
    
    let numBarrels = 6.
    let barrelSpread = 50.
    let barrels = [ for i in 0. .. numBarrels -> { barrel with pos = Vec2.fromAngle(i * (360./numBarrels)) |> Vec2.mul barrelSpread } ]
    
    let gameData = { player = player; barrels = barrels; enemies = [enemy] }
    
    let app = new Application ("FSharpRipOff", (800, 600))
    app.setOnUpdate (update app gameData)
    app.run ()

main ()
