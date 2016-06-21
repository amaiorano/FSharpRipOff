open OpenTKPlatform
open System

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
let cossin rads = (cos (rads), sin (rads))
let twoPI = 2. * Math.PI
let normalizeAngle2PI rads = Math.IEEERemainder(rads, twoPI)

let normalizeAnglePI rads = 
    match (normalizeAngle2PI rads) with
    | x when x <= Math.PI -> x
    | x -> twoPI - x

module Vec2 = 
    type T = float * float
    
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
    let mul s (x, y) = (x * s, y * s)
    let addMul (x, y) (dx, dy) s = (x + dx * s, y + dy * s)
    let fromAngle rads = cossin (rads)
    let magnitude (x, y) = Math.Sqrt(x * x + y * y)
    
    let normalize (x, y) = 
        let mag = magnitude (x, y)
        assert (mag <> 0.)
        (x / mag, y / mag)
    
    let dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
    let cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2
    let distance v1 v2 = magnitude (sub v1 v2)
    // returns rads in [0,PI]
    let shortestAngleDiff v1 v2 = Math.Acos(dot (normalize (v1)) (normalize (v2)))
    // returns angle in [-PI,PI]
    let signedAngleDiff v1 v2 = Math.Atan2(cross v1 v2, dot v1 v2)

type Vec2List = Vec2.T list

// Game-specific
let screenSize = 800., 600.
let screenRect = (-fst (screenSize) / 2., -snd (screenSize) / 2., fst (screenSize) / 2., snd (screenSize) / 2.)

type Visual = 
    { vertices : Vec2List list }

type Actor = 
    { pos : Vec2.T
      angle : float // rads
      speed : float
      visual : Visual }

let defaultActor = 
    { pos = (0., 0.)
      angle = 0.
      speed = 0.
      visual = { vertices = [ [] ] } }

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

let drawVisual (canvas : Canvas) (vis : Visual) = vis.vertices |> List.iter canvas.drawVertices

let drawActor (canvas : Canvas) (actor : Actor) = 
    canvas.save()
    canvas.translate actor.pos
    canvas.rotate (actor.angle)
    drawVisual canvas actor.visual
    canvas.restore()

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
        f v (0., 0., 0., 0.)
    
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
        { vertices = [ body; wheel1; wheel2; turret ] }
    
    let makeEnemy() = 
        let body = square |> scale (25., 16.)
        let body2 = circle 8 |> scaleUni 5.
        
        let turret = 
            triangle
            |> scale (10., 4.)
            |> translate (12., 0.)
        
        let turret1 = turret |> translate (0., -5.)
        let turret2 = turret |> translate (0., 5.)
        { vertices = [ body; body2; turret1; turret2 ] }
    
    let makeBarrel() = { vertices = [ circle 6 |> scaleUni 10. ] }
    let makeBullet() = { vertices = [ line 2. ] }

let defaultBullet : Bullet = { defaultActor with visual = GameVisual.makeBullet() }

type GameData = 
    { player : Player
      enemies : Enemy list
      barrels : Barrel list
      bullets : Bullet list
      bulletFireTimeOut : float }

let allActors gameData = 
    [ gameData.player ] 
    @ gameData.barrels 
      @ (gameData.enemies |> List.map (fun e -> e.actor)) 
        @ (gameData.enemies |> List.choose (fun e -> e.barrel)) @ gameData.bullets

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

let bulletSpeed = 400.
let bullFireInterval = 0.2
let barrelStartCount = 6
let barrelStartPosRadius = 50.

let integrate speed angle dt pos = 
    let moveDelta = speed * dt
    let forward = Vec2.fromAngle angle
    Vec2.addMul pos forward moveDelta

let isNearPosition pos1 pos2 = Vec2.distance pos1 pos2 < 10.

let movePlayer dt player = 
    let actor = player
    
    let turnAction = 
        if Keyboard.IsDown Key.Left then TurnLeft
        elif Keyboard.IsDown Key.Right then TurnRight
        else NoTurn
    
    let turnRate = twoPI
    let finalAngle = updateFacingAngle turnRate turnAction dt actor.angle
    
    let speedAction = 
        if Keyboard.IsDown Key.Up then Accelerate
        elif Keyboard.IsDown Key.Down then Break
        else Decelerate
    
    let speedConstants = playerSpeedConstants
    let finalSpeed = updateSpeed speedConstants speedAction dt actor.speed
    let finalPos = integrate finalSpeed finalAngle dt actor.pos
    { player with pos = finalPos
                  angle = finalAngle
                  speed = finalSpeed }

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

let updateEnemy (dt : float) (player : Player) (enemy : Enemy) = 
    let enemy = 
        match enemy.state with
        | Idle -> 
            match enemy.barrel with
            | Some(barrel) -> { enemy with state = GrabBarrel }
            | None -> { enemy with state = AttackPlayer }
        | GrabBarrel -> 
            let barrel = enemy.barrel.Value
            if isNearPosition (enemy.actor.pos) (barrel.pos) then { enemy with state = Leave }
            else moveEnemy barrel.pos dt enemy
        | Leave -> 
            if isNearPosition (enemy.actor.pos) (enemy.spawnPos) then enemy //@TODO "destroy enemy and barrel"
            else 
                let e = moveEnemy enemy.spawnPos dt enemy
                let barrel = { e.barrel.Value with pos = e.actor.pos }
                { e with barrel = Some(barrel) }
        | AttackPlayer -> moveEnemy (player.pos) dt enemy
    enemy

let makeEnemyWave() = 
    let numEnemies = 2
    let enemy = { defaultEnemy with actor = { defaultActor with visual = GameVisual.makeEnemy() } }
    // Spawn at points around the screen rect
    let offset = 30.
    
    let spawnRect = 
        match screenRect with
        | (left, bottom, right, top) -> left - offset, bottom - offset, right + offset, top + offset
    
    let positions = 
        Shape.circle numEnemies
        |> Shape.scaleUni 1000.
        |> Shape.shrinkToRect spawnRect
    
    [ for i in 0..positions.Length - 1 -> 
          { enemy with spawnPos = positions.[i]
                       actor = { enemy.actor with pos = positions.[i] } } ]

let enemyEscaped enemy = enemy.state = Leave && isNearPosition enemy.actor.pos enemy.spawnPos
let allEnemiesDead (enemies : Enemy list) = enemies.IsEmpty

let intersectsPointRect point rect = 
    let x, y = point
    let x1, y1, x2, y2 = rect
    x >= x1 && x <= x2 && y >= y1 && y <= y2

let isActorOnScreen (actor : Actor) = intersectsPointRect actor.pos screenRect

// Returns sequence of tuples (x,y) for which predicate returns true for all permutations of list1 and list2
let choosePermute2 predicate list1 list2 = 
    let len1 = List.length list1
    let len2 = List.length list2
    let tuples = Seq.init (len1 * len2) (fun i -> list1.[i / len2], list2.[i % len2])
    tuples |> Seq.filter predicate

let rec update (app : Application) (gameData : GameData) (dt : float) (canvas : Canvas) = 
    // Logic update
    // If all enemies dead, create new wave
    let enemies, barrels = 
        if allEnemiesDead gameData.enemies then 
            let enemies = makeEnemyWave()
            // assign barrels from master list to new wave
            let barrels = gameData.barrels
            let num = Math.Min(enemies.Length, barrels.Length)
            
            let enemies = 
                enemies |> List.mapi (fun i e -> 
                               if i < num then { e with barrel = Some(barrels.[i]) }
                               else e)
            
            let barrels = barrels |> List.skip num // Remove assigned barrels
            (enemies, barrels)
        else (gameData.enemies, gameData.barrels)
    
    let gameData = 
        { gameData with enemies = enemies
                        barrels = barrels }
    
    // Update player
    let player = gameData.player |> movePlayer dt
    // Update bullets
    let bulletFireTimeOut = max 0. (gameData.bulletFireTimeOut - dt)
    
    let newBullet = 
        if bulletFireTimeOut = 0. && Keyboard.IsDown Key.Space then 
            Some { defaultBullet with pos = player.pos
                                      angle = player.angle }
        else None
    
    let bulletFireTimeOut = 
        if bulletFireTimeOut = 0. then bullFireInterval
        else bulletFireTimeOut
    
    let bullets = gameData.bullets @ Option.toList newBullet
    
    let bullets = 
        bullets
        |> List.map (fun b -> { b with pos = integrate bulletSpeed b.angle dt b.pos })
        |> List.filter isActorOnScreen
    
    // Update enemies
    let enemies = gameData.enemies |> List.map (updateEnemy dt player)
    // Check for enemy-bullet collisions
    let collisionTest (bullet, enemy) = Vec2.distance bullet.pos enemy.actor.pos < 30.
    let collisions = choosePermute2 collisionTest bullets enemies
    
    let deadBullets = 
        seq { 
            for t in collisions -> fst (t)
        }
    
    let deadEnemies = 
        seq { 
            for t in collisions -> snd (t)
        }
    
    // Put barrels of dead enemies back into master list
    let barrelsReleased = 
        deadEnemies
        |> Seq.choose (fun e -> e.barrel)
        |> List.ofSeq
    
    // Remove dead bullets and enemies from master lists
    let bullets = set bullets - set deadBullets |> List.ofSeq
    let enemies = set enemies - set deadEnemies |> List.ofSeq
    let barrels = gameData.barrels @ barrelsReleased
    // If enemies escaped, remove enemy (along with its barrel)
    let enemies = enemies |> List.filter (fun e -> not (enemyEscaped e))
    
    // To keep it pure, we need to re-register a new instance of update that
    // binds the updated gameData
    let gameData = 
        { gameData with player = player
                        enemies = enemies
                        barrels = barrels
                        bullets = bullets
                        bulletFireTimeOut = bulletFireTimeOut }
    app.setOnUpdate (update app gameData)
    // Render
    canvas.resetTransform()
    canvas.clear()
    allActors gameData |> List.iter (fun actor -> drawActor canvas actor)

let main() = 
    let player = 
        { defaultActor with pos = (120., 40.)
                            angle = degToRad 120.
                            visual = GameVisual.makeTank() }
    
    let barrel = { defaultActor with visual = GameVisual.makeBarrel() }
    let barrelPositions = Shape.circle barrelStartCount |> Shape.scaleUni barrelStartPosRadius
    
    let barrels = 
        [ for i in 1..barrelPositions.Length -> { barrel with pos = barrelPositions.[i - 1] } ]
    
    let gameData = 
        { player = player
          barrels = barrels
          enemies = []
          bullets = []
          bulletFireTimeOut = 0. }
    
    let app = new Application("FSharpRipOff", screenSize |> castTuple int)
    app.setOnUpdate (update app gameData)
    app.run()

main()
