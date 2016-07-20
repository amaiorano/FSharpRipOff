﻿open OpenTKPlatform
open System

//@TODO: add these to Seq module?
let filterOut predicate seq = seq |> Seq.filter (fun x -> not (predicate x))
let appendSingleton sequence x = Seq.singleton x |> Seq.append sequence

let removeOuterList (list : 'a list list) = 
    [ for x in list do
          yield! x ]

let swapArgs f a b = f b a
let castTuple t (a, b) = (t a, t b)
let min (x : float) (y : float) = Math.Min(x, y)
let max (x : float) (y : float) = Math.Max(x, y)

let clamp minV maxV v = 
    v
    |> max minV
    |> min maxV

let cos = Math.Cos
let sin = Math.Sin
let atan2 = Math.Atan2
let cossin rads = (cos (rads), sin (rads))
let twoPI = 2. * Math.PI
let normalizeAngle2PI rads = Math.IEEERemainder(rads, twoPI)

let normalizeAnglePI rads = 
    match (normalizeAngle2PI rads) with
    | x when x <= Math.PI -> x
    | x -> twoPI - x

module Vec2 = 
    type T = float * float
    
    let zero = 0., 0.
    let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2
    let sub (x1, y1) (x2, y2) = x1 - x2, y1 - y2
    let elementWiseMul (x1, y1) (x2, y2) = x1 * x2, y1 * y2
    let mul s (x, y) = x * s, y * s
    let addMul (x, y) (dx, dy) s = x + dx * s, y + dy * s
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

type Vec2List = Vec2.T list

// OpenTK math types
//@TODO: should we depend on these? If so, add Vector3 and Matrix4 to OpenTKPlatform to get rid of the namespace
let toVector3 pos = OpenTK.Vector3(float32 (fst pos), float32 (snd pos), 0.f)
let fromVector3 (vec3 : OpenTK.Vector3) = (float vec3.X, float vec3.Y)

let toMatrix4 pos angle = 
    let mt = OpenTK.Matrix4.CreateTranslation(toVector3 pos)
    let mr = OpenTK.Matrix4.CreateRotationZ(float32 angle)
    mr * mt

let isPointInsideRect point rect = 
    let x, y = point
    let x1, y1, x2, y2 = rect
    x >= x1 && x <= x2 && y >= y1 && y <= y2

// Returns sequence of tuples (x,y) for which predicate returns true for all permutations of seq1 and seq2
let choosePermute2 predicate seq1 seq2 = 
    (Seq.empty, seq1) ||> Seq.fold (fun tuples e1 -> 
                              (tuples, seq2) ||> Seq.fold (fun tuples e2 -> 
                                                     if predicate (e1, e2) then appendSingleton tuples (e1, e2)
                                                     else tuples))

// Returns intersection of lines ps1,pe1 and ps2,pe2
let intersection (ps1x, ps1y) (pe1x, pe1y) (ps2x, ps2y) (pe2x, pe2y) = 
    let A1 = pe1y - ps1y
    let B1 = ps1x - pe1x
    let C1 = A1 * ps1x + B1 * ps1y
    let A2 = pe2y - ps2y
    let B2 = ps2x - pe2x
    let C2 = A2 * ps2x + B2 * ps2y
    let delta = A1 * B2 - A2 * B1
    //    if delta = 0. then None
    assert (delta <> 0.)
    (B2 * C1 - B1 * C2) / delta, (A1 * C2 - A2 * C1) / delta

type Approach = 
    // Approaches current towards target using a viscous damped approach; by default, it will reach
    // 99% (factor) of target within 1 (timeToTarget) second, then will keep getting closer but never
    // reach the (asymptotic) target. The tolerance can be set to make sure the target is reached, but
    // keep in mind that this modifies the shape of the curve (slightly).
    static member damped (current, target, dt, ?factor, ?timeToTarget, ?tolerance) = 
        let factor = defaultArg factor 0.99
        let timeToTarget = defaultArg timeToTarget 1.
        let tolerance = defaultArg tolerance 0.
        if current = target || timeToTarget = 0. then target
        else 
            let delta = target - current
            let deltaSign = float (Math.Sign(float32 delta))
            let newTarget = target + deltaSign * tolerance
            let alpha = 1. - Math.Pow(1. - factor, dt / timeToTarget)
            let step = (newTarget - current) * alpha
            if (step * deltaSign) > (delta * deltaSign) then target
            else current + step

// Game-specific
let screenSize = 800., 600.
// order: left, bottom, right, top
let screenRect = (-fst (screenSize) / 2., -snd (screenSize) / 2., fst (screenSize) / 2., snd (screenSize) / 2.)

let screenEdges = 
    let (l, b, r, t) = screenRect
    [ (l, t)
      (l, b) ], 
    [ (l, b)
      (r, b) ], 
    [ (r, b)
      (r, t) ], 
    [ (r, t)
      (l, t) ]

type Visual = 
    { // List of vertex lists, each of which will be drawn as a line loop
      vertLists : Vec2List list }

type Actor = 
    { pos : Vec2.T
      angle : float // rads
      speed : float
      visual : Visual }

let defaultActor = 
    { pos = (0., 0.)
      angle = 0.
      speed = 0.
      visual = { vertLists = [ [] ] } }

type Player = Actor

type Barrel = Actor

type Bullet = Actor

type EnemyState = 
    | Idle
    | GrabBarrel
    | Leave
    | AttackPlayer

type Enemy = 
    { actor : Actor
      spawnPos : Vec2.T
      state : EnemyState
      barrel : Barrel option }

let defaultEnemy = 
    { actor = defaultActor
      spawnPos = (0., 0.)
      state = Idle
      barrel = None }

type Destruction = 
    { actor : Actor
      hitPos : Vec2.T
      elapsedTime : float }

let defaultDestruction = 
    { actor = defaultActor
      hitPos = Vec2.zero
      elapsedTime = 0. }

let drawVisual (canvas : Canvas) (vis : Visual) = vis.vertLists |> List.iter canvas.drawVertices

let drawActor (canvas : Canvas) (actor : Actor) = 
    canvas.save()
    canvas.translate actor.pos
    canvas.rotate (actor.angle)
    canvas.color 1. 1. 1.
    drawVisual canvas actor.visual
    canvas.restore()

// Transform world space v into local space of actor
let toActorLocalSpace (actor : Actor) (v : Vec2.T) = 
    let m = toMatrix4 actor.pos actor.angle
    m.Invert()
    let vf = OpenTK.Vector3.Transform(toVector3 v, m)
    fromVector3 vf

let toActorWorldSpace (actor : Actor) (v : Vec2.T) = 
    let m = toMatrix4 actor.pos actor.angle
    let vf = OpenTK.Vector3.Transform(toVector3 v, m)
    fromVector3 vf

module Shape = 
    let scale (x, y) verts = verts |> List.map (fun (x', y') -> (x' * x, y' * y))
    let scaleUni s verts = scale (s, s) verts
    let translate (x, y) verts = verts |> List.map (fun (x', y') -> (x' + x, y' + y))
    let translateInv (x, y) verts = translate (-x, -y) verts
    
    // returns (minX,minY,maxX,maxY)
    let extents (v : Vec2List) = 
        let rec f (v : Vec2List) (minX, minY, maxX, maxY) = 
            match v with
            | [] -> (minX, minY, maxX, maxY)
            | (hx, hy) :: tail -> f tail (min hx minX, min hy minY, max hx maxX, max hy maxY)
        
        let (minX, minY), (maxX, maxY) = v.[0], v.[1]
        f v (minX, minY, maxX, maxY)
    
    // returns center point of input extents (as Vec2)
    let midpoint (v : Vec2List) = 
        let (minX, minY, maxX, maxY) = extents v
        (minX + (maxX - minX) / 2., minY + (maxY - minY) / 2.)
    
    // returns new Vec2List translated so that its midpoint is (0,0) in local space
    let center (v : Vec2List) = translateInv (midpoint v) v
    let shrinkToRect (minX, minY, maxX, maxY) (v : Vec2List) = 
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

module GameVisual = 
    open Shape
    
    let makeTank() = 
        let body = square |> scale (28., 16.)
        
        let turret = 
            triangle
            |> scale (20., 4.)
            |> translate (5., 0.)
        
        let buildWheels size num = 
            [ for i in 0..num - 1 do
                  yield! translate (float i * size, 0.) (scaleUni size square) ]
            |> center
        
        let wheels = buildWheels 6. 5
        let wheel1 = wheels |> translate (0., -13.)
        let wheel2 = wheels |> translate (0., 13.)
        { vertLists = [ body; wheel1; wheel2; turret ] }
    
    let makeEnemy() = 
        let body = square |> scale (25., 16.)
        let body2 = circle 8 |> scaleUni 5.
        
        let turret = 
            triangle
            |> scale (10., 4.)
            |> translate (12., 0.)
        
        let turret1 = turret |> translate (0., -5.)
        let turret2 = turret |> translate (0., 5.)
        { vertLists = [ body; body2; turret1; turret2 ] }
    
    let makeBarrel() = { vertLists = [ circle 6 |> scaleUni 10. ] }
    let makeBullet() = { vertLists = [ square |> scale (4., 2.) ] }

let defaultBullet : Bullet = { defaultActor with visual = GameVisual.makeBullet() }

module CameraShake = 
    type T = 
        { rand : Random
          amplitude : float }
    
    let defaultCameraShake = 
        { rand = new Random()
          amplitude = 0. }
    
    let maxAmplitude = 10.
    let amplitudeIncrement = 3.
    let amplitudeDecayTime = 1.2
    let shake camShake = { camShake with amplitude = min maxAmplitude (camShake.amplitude + amplitudeIncrement) }
    let shakeN numTimes camShake = { 1..numTimes } |> Seq.fold (fun camShake n -> shake camShake) camShake
    let update dt camShake = 
        { camShake with amplitude = 
                            Approach.damped 
                                (camShake.amplitude, 0., dt, timeToTarget = amplitudeDecayTime, tolerance = 0.05) }
    let getOffset (camShake : T) = 
        (camShake.rand.NextDouble() * camShake.amplitude, camShake.rand.NextDouble() * camShake.amplitude)

type GameData = 
    { players : Player list
      playerSpawnTimeOut : float
      enemies : Enemy list
      barrels : Barrel list
      bullets : Bullet list
      bulletFireTimeOut : float
      destructions : Destruction list
      cameraShake : CameraShake.T }

let allActors gameData = 
    gameData.players 
    @ gameData.barrels 
      @ (gameData.enemies |> List.map (fun e -> e.actor)) 
        @ (gameData.enemies |> List.choose (fun e -> e.barrel)) 
          @ gameData.bullets @ (gameData.destructions |> List.map (fun d -> d.actor))

type SpeedConstants = 
    { speedUpRate : float
      speedDownRate : float
      autoSpeedDownRate : float
      maxSpeed : float }

type SpeedAction = 
    | Decelerate
    | Accelerate
    | Break
    | Stop

let updateSpeed (c : SpeedConstants) action dt speed = 
    let speedDelta = 
        match action with
        | Decelerate -> c.autoSpeedDownRate
        | Accelerate -> c.speedUpRate
        | Break -> c.speedDownRate
        | Stop -> -(speed / dt) // Instant stop
    
    let newSpeed = speed + speedDelta * dt |> clamp 0. c.maxSpeed
    newSpeed

type TurnAction = 
    | NoTurn
    | TurnLeft
    | TurnRight

let updateFacingAngle turnRate action dt angle = 
    let angleDelta = 
        match action with
        | NoTurn -> 0.
        | TurnLeft -> turnRate
        | TurnRight -> -turnRate
    
    let finalAngle = angle + angleDelta * dt
    finalAngle

// Game constants
let playerSpeedConstants = 
    { speedUpRate = 400.
      speedDownRate = -800.
      autoSpeedDownRate = -300.
      maxSpeed = 250. }

let enemySpeedConstants = 
    { speedUpRate = 300.
      speedDownRate = -800.
      autoSpeedDownRate = -300.
      maxSpeed = 150. }

let playerSpawnInterval = 1.5
let bulletSpeed = 400.
let bullFireInterval = 0.2
let barrelStartCount = 6
let barrelStartPosRadius = 50.
let destructionTime = 2.
let destructionMoveSpeed = 200.

module GameInput = 
    let TurnLeft() = Keyboard.IsAnyDown [ Key.Left; Key.A ]
    let TurnRight() = Keyboard.IsAnyDown [ Key.Right; Key.D ]
    let Accelerate() = Keyboard.IsAnyDown [ Key.Up; Key.W ]
    let Break() = Keyboard.IsAnyDown [ Key.Down; Key.S ]
    let Fire() = Keyboard.IsAnyDown [ Key.Space; Key.J; Key.LControl ]

let integrate speed angle dt pos = 
    let moveDelta = speed * dt
    let forward = Vec2.fromAngle angle
    Vec2.addMul pos forward moveDelta

let isNearPosition pos1 pos2 = Vec2.distance pos1 pos2 < 10.

let movePlayer dt player = 
    let actor = player
    
    let turnAction = 
        if GameInput.TurnLeft() then TurnLeft
        elif GameInput.TurnRight() then TurnRight
        else NoTurn
    
    let turnRate = twoPI
    let finalAngle = updateFacingAngle turnRate turnAction dt actor.angle
    
    let speedAction = 
        if GameInput.Accelerate() then Accelerate
        elif GameInput.Break() then Break
        else Decelerate
    
    let speedConstants = playerSpeedConstants
    let finalSpeed = updateSpeed speedConstants speedAction dt actor.speed
    let finalPos = integrate finalSpeed finalAngle dt actor.pos
    { player with pos = finalPos
                  angle = finalAngle
                  speed = finalSpeed }

let makePlayer() = 
    { defaultActor with pos = (120., 40.)
                        angle = degToRad 120.
                        visual = GameVisual.makeTank() }

let moveEnemy (targetPos : Vec2.T) dt (enemy : Enemy) = 
    let actor = enemy.actor
    let toTarget = Vec2.sub targetPos (actor.pos)
    let forward = Vec2.fromAngle actor.angle
    let angleDelta = (Vec2.signedAngleDiff forward toTarget)
    
    let turnAction = 
        match angleDelta with
        | x when x > 0. -> TurnLeft
        | x when x < 0. -> TurnRight
        | _ -> NoTurn
    
    let turnRate = 2. * twoPI * ((Math.Abs angleDelta) / Math.PI)
    let finalAngle = updateFacingAngle turnRate turnAction dt actor.angle
    
    let speedAction = 
        match (Vec2.magnitude toTarget) with
        | x when x > 30. -> Accelerate
        | x when x > 5. -> Decelerate
        | _ -> Stop
    
    let speedConstants = enemySpeedConstants
    let finalSpeed = updateSpeed speedConstants speedAction dt actor.speed
    let finalPos = integrate finalSpeed finalAngle dt actor.pos
    { enemy with actor = 
                     { enemy.actor with pos = finalPos
                                        angle = finalAngle
                                        speed = finalSpeed } }

let updateEnemy (dt : float) (players : Player seq) (enemy : Enemy) = 
    let enemy = 
        match enemy.state with
        | Idle -> 
            match enemy.barrel with
            | Some(barrel) -> { enemy with state = GrabBarrel }
            | None -> 
                if Seq.isEmpty players then 
                    // circle around until player spawns
                    let turnRate = 10. * Math.PI
                    let angle = enemy.actor.angle + turnRate * dt
                    let pointAhead = Vec2.add enemy.actor.pos (Vec2.mul 100. (Vec2.fromAngle angle))
                    moveEnemy pointAhead dt enemy
                else { enemy with state = AttackPlayer }
        | GrabBarrel -> 
            let barrel = enemy.barrel.Value
            if isNearPosition (enemy.actor.pos) (barrel.pos) then { enemy with state = Leave }
            else moveEnemy barrel.pos dt enemy
        | Leave -> 
            let e = moveEnemy enemy.spawnPos dt enemy
            let barrel = { e.barrel.Value with pos = e.actor.pos }
            { e with barrel = Some(barrel) }
        | AttackPlayer -> 
            if Seq.isEmpty players then { enemy with state = Idle }
            else moveEnemy ((Seq.head players).pos) dt enemy //@TODO: per player
    enemy

let makeEnemyWave() = 
    let numEnemies = 5
    let enemy = { defaultEnemy with actor = { defaultActor with visual = GameVisual.makeEnemy() } }
    // Spawn at points around the screen rect
    let offset = 30.
    
    let spawnRect = 
        let left, bottom, right, top = screenRect
        left - offset, bottom - offset, right + offset, top + offset
    
    let positions = 
        Shape.circle numEnemies
        |> Shape.scaleUni 1000.
        |> Shape.shrinkToRect spawnRect
    
    seq { 
        for i in 0..positions.Length - 1 -> 
            { enemy with spawnPos = positions.[i]
                         actor = { enemy.actor with pos = positions.[i] } }
    }

let enemyEscaped enemy = enemy.state = Leave && isNearPosition enemy.actor.pos enemy.spawnPos
let allEnemiesDead enemies = Seq.isEmpty enemies
let isActorOnScreen (actor : Actor) = isPointInsideRect actor.pos screenRect

let bounceOffScreenEdge (preMoveActor : Actor) (postMoveActor : Actor) : Actor = 
    let x1, y1 = preMoveActor.pos
    let x2, y2 = postMoveActor.pos
    
    let computeReflectedPosAndAngle (edge : Vec2List) reflectDir = 
        let p = intersection (x1, y1) (x2, y2) edge.[0] edge.[1]
        let p' = Vec2.sub postMoveActor.pos p // intersection point to new pos
        let delta = Vec2.elementWiseMul p' reflectDir
        Vec2.add p delta, Vec2.toAngle delta
    
    let left, bottom, right, top = screenRect
    let leftEdge, bottomEdge, rightEdge, topEdge = screenEdges
    let reflectDirX = (-1., 1.)
    let reflectDirY = (1., -1.)
    
    let pos, angle = 
        if x2 < left then computeReflectedPosAndAngle leftEdge reflectDirX
        elif x2 > right then computeReflectedPosAndAngle rightEdge reflectDirX
        elif y2 > top then computeReflectedPosAndAngle topEdge reflectDirY
        elif y2 < bottom then computeReflectedPosAndAngle bottomEdge reflectDirY
        else postMoveActor.pos, postMoveActor.angle
    { postMoveActor with pos = pos
                         angle = angle }

let convertToDestruction (hitPos : Vec2.T) (actor : Actor) = 
    // create single list of lines
    let vertLists = actor.visual.vertLists
    
    // Break apart each vert list into list of lines
    //@TODO: move to a Visual module function
    let vertLists = 
        vertLists
        |> List.map (fun verts -> 
               verts @ [ verts.[0] ] //@TODO: inefficient!
               |> List.pairwise
               |> List.map (fun (x, y) -> [ x; y ]))
        |> removeOuterList
    { defaultDestruction with hitPos = hitPos
                              actor = { actor with visual = { vertLists = vertLists } } }

let updateDestructions dt (destructions : Destruction seq) = 
    let updateDestruction (d : Destruction) : Destruction = 
        let hitPosLS = 
            d.hitPos
            |> toActorLocalSpace d.actor
            |> Vec2.normalize
            |> Vec2.mul 20.
        
        let vertLists = 
            d.actor.visual.vertLists |> List.map (fun verts -> 
                                            let midpoint = (Shape.midpoint verts)
                                            let moveDir = Vec2.sub (Shape.midpoint verts) hitPosLS |> Vec2.normalize
                                            // Move pieces slower as they approach their destruction time - but get there a little faster
                                            // so they can remain on the ground for a while before disappearing
                                            let ratio = 
                                                max 0. ((destructionTime - d.elapsedTime * 2.) / destructionTime)
                                            let offset = Vec2.mul (ratio * destructionMoveSpeed * dt) moveDir
                                            Shape.translate offset verts)
        
        { d with actor = { d.actor with visual = { d.actor.visual with vertLists = vertLists } } } //@TODO: better way?
    destructions
    |> Seq.map updateDestruction
    |> Seq.map (fun d -> { d with elapsedTime = d.elapsedTime + dt })
    |> Seq.filter (fun d -> d.elapsedTime < destructionTime)

let rec update (app : Application) (gameData : GameData) (dt : float) (canvas : Canvas) = 
    let players = gameData.players |> List.toSeq
    let playerSpawnTimeOut = gameData.playerSpawnTimeOut
    let enemies = gameData.enemies |> List.toSeq
    let barrels = gameData.barrels |> List.toSeq
    let bullets = gameData.bullets |> List.toSeq
    let bulletFireTimeOut = gameData.bulletFireTimeOut
    let destructions = gameData.destructions |> List.toSeq
    let cameraShake = gameData.cameraShake
    let lastDestructions = destructions
    
    // Logic update
    //@TODO: Respawn dead players after some time
    let players, playerSpawnTimeOut = 
        if Seq.isEmpty players then 
            let playerSpawnTimeOut = max 0. (playerSpawnTimeOut - dt)
            match playerSpawnTimeOut with
            | 0. -> Seq.singleton (makePlayer()), playerSpawnInterval
            | _ -> players, playerSpawnTimeOut
        else players, playerSpawnTimeOut
    
    // If all enemies dead, create new wave
    let enemies, barrels = 
        if allEnemiesDead enemies then 
            let enemies = makeEnemyWave()
            // assign barrels from master list to new wave
            let numEnemiesToAssignBarrelsTo = Math.Min(Seq.length enemies, Seq.length barrels)
            let enemiesLeft = enemies |> Seq.skip numEnemiesToAssignBarrelsTo
            // starting with enemies we don't assign barrels to (enemiesLeft), we fold into it enemies with barrels
            let enemies = 
                Seq.fold2 (fun enemies e b -> appendSingleton enemies { e with barrel = Some(b) }) enemiesLeft enemies 
                    barrels
            let barrels = barrels |> Seq.skip numEnemiesToAssignBarrelsTo
            (enemies, barrels)
        else (enemies, barrels)
    
    // Update players
    let players = 
        players |> Seq.map (fun p -> 
                       p
                       |> movePlayer dt
                       |> bounceOffScreenEdge p) // 'p' here is the pre-moved copy
    
    // Update bullets - @TODO: per player
    let bulletFireTimeOut = max 0. (bulletFireTimeOut - dt)
    let fireBullet = bulletFireTimeOut = 0. && GameInput.Fire()
    
    let bullets = 
        players |> Seq.fold (fun bullets p -> 
                       if fireBullet then 
                           appendSingleton bullets { defaultBullet with pos = p.pos
                                                                        angle = p.angle }
                       else bullets) bullets
    
    //@TODO: per player
    let bulletFireTimeOut = 
        if fireBullet then bullFireInterval
        else bulletFireTimeOut
    
    let bullets = 
        bullets
        |> Seq.map (fun b -> { b with pos = integrate bulletSpeed b.angle dt b.pos })
        |> Seq.filter isActorOnScreen
    
    // Update enemies
    let enemies = enemies |> Seq.map (updateEnemy dt players)
    // Handle collisions
    let addTupleToSets (set1 : Set<'a>, set2 : Set<'b>) (x, y) = set1.Add(x), set2.Add(y)
    
    let toActor (x : obj) = 
        match x with
        | :? Enemy as e -> e.actor
        | :? Actor as a -> a
        | _ -> 
            assert false
            defaultActor // @TODO: what should we do here?
    
    let collisionPairToDestruction (x, y) = convertToDestruction (toActor x).pos (toActor y)
    let collisionTest (actor1, actor2) = Vec2.distance actor1.pos actor2.pos < 30.
    let collisionTestWithEnemy (actor, enemy : Enemy) = collisionTest (actor, enemy.actor)
    
    let collisionsToDestructions destructions collisions = 
        collisions |> Seq.fold (fun destructions (x, y) -> 
                          seq { 
                              yield collisionPairToDestruction (x, y)
                              yield collisionPairToDestruction (y, x)
                              yield! destructions
                          }) destructions
    
    let deadBullets = Set<Bullet>([])
    let deadEnemies = Set<Enemy>([])
    let deadPlayers = Set<Player>([])
    // Bullet-enemy collisions
    let bulletEnemyCollisions = choosePermute2 collisionTestWithEnemy bullets enemies
    let deadBullets, deadEnemies = bulletEnemyCollisions |> Seq.fold addTupleToSets (deadBullets, deadEnemies)
    let destructions = collisionsToDestructions destructions bulletEnemyCollisions
    // Player-enemy collisions    
    let playerEnemyCollisions = choosePermute2 collisionTestWithEnemy players enemies
    let deadPlayers, deadEnemies = playerEnemyCollisions |> Seq.fold addTupleToSets (deadPlayers, deadEnemies)
    let destructions = collisionsToDestructions destructions playerEnemyCollisions
    // Remove dead actors
    let players = players |> filterOut deadPlayers.Contains
    let enemies = enemies |> filterOut deadEnemies.Contains
    let bullets = bullets |> filterOut deadBullets.Contains
    
    // Put barrels of dead enemies back into master list
    let barrels = 
        deadEnemies
        |> Seq.choose (fun e -> e.barrel) // Returns list of dead enemy barrels
        |> Seq.fold (fun barrels deadEnemyBarrel -> appendSingleton barrels deadEnemyBarrel) barrels
    
    // If enemies escaped, remove enemy (along with its barrel)
    let enemies = enemies |> Seq.filter (fun e -> not (enemyEscaped e))
    //  Update destructions
    let numNewDestructions = Seq.length destructions - Seq.length lastDestructions
    let destructions = destructions |> updateDestructions dt
    // Update camera shake
    let cameraShake = CameraShake.shakeN numNewDestructions cameraShake
    let cameraShakeOffset = CameraShake.getOffset cameraShake
    let cameraShake = CameraShake.update dt cameraShake
    
    // To keep it pure, we need to re-register a new instance of update that
    // binds the updated gameData
    let gameData = 
        { gameData with players = players |> List.ofSeq
                        playerSpawnTimeOut = playerSpawnTimeOut
                        enemies = enemies |> List.ofSeq
                        barrels = barrels |> List.ofSeq
                        bullets = bullets |> List.ofSeq
                        bulletFireTimeOut = bulletFireTimeOut
                        destructions = destructions |> List.ofSeq
                        cameraShake = cameraShake }
    app.setOnUpdate (update app gameData)
    // Render
    canvas.resetTransform()
    canvas.clear()
    canvas.translate cameraShakeOffset
    allActors gameData |> List.iter (fun actor -> drawActor canvas actor)

let main() = 
    let barrel = { defaultActor with visual = GameVisual.makeBarrel() }
    let barrelPositions = Shape.circle barrelStartCount |> Shape.scaleUni barrelStartPosRadius
    
    let barrels = 
        [ for i in 1..barrelPositions.Length -> { barrel with pos = barrelPositions.[i - 1] } ]
    
    let gameData = 
        { players = []
          playerSpawnTimeOut = 0.
          barrels = barrels
          enemies = []
          bullets = []
          bulletFireTimeOut = 0.
          destructions = []
          cameraShake = CameraShake.defaultCameraShake }
    
    let app = new Application("FSharpRipOff", screenSize |> castTuple int)
    app.setOnUpdate (update app gameData)
    app.run()

main()
