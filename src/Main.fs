#if !INTERACTIVE
module Main
open OpenTKPlatform
#endif

//#if INTERACTIVE
//#load "OpenTKPlatform.fs"
//#endif

let degToRad degs = float(degs) * System.Math.PI / 180.0

type Visual = {
    vertices:(float*float) list
}

type Actor = {
    pos:(float*float)
    angle:float
    visual:Visual
}

let defaultActor = {
    pos = (0.,0.)
    angle = 0.
    visual = { vertices = [] }
}

let drawVisual (canvas:Canvas) (vis:Visual) =
    canvas.drawVertices vis.vertices

let drawActor (canvas:Canvas) (actor:Actor) =
    canvas.save ()
    canvas.translate actor.pos
    canvas.rotate (degToRad actor.angle)
    drawVisual canvas actor.visual
    canvas.restore ()

let makeShipVisual () =
    let scale = 10.
    let verts = [ (-1.,-1.); (0.,2.); (1.,-1.) ] |> List.map (fun (x,y) -> (x*scale, y*scale))
    { vertices=verts }

let add ((p1x,p1y):float*float) ((p2x,p2y):float*float) =
    (p1x + p2x, p1y + p2y)


let rotateActor degs actor = {actor with angle = actor.angle + degs}

let rec update (app:Application) (actors:Actor list) dt (canvas:Canvas) =
    // Logic update
    let player = actors.[0]
//    let player = { player with pos = add player.pos (keyboard.arrows()) }
    let others = List.tail actors |> List.map (rotateActor 100.)
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
    
    let app = new Application("FSharpRipOff")
    app.setOnUpdate (update app actors)
    app.run ()

main ()
