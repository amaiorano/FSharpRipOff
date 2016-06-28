module OpenTKPlatform

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

let degToRad (degs : float) = degs * System.Math.PI / 180.0
let radToDeg (rads : float) = rads * 180.0 / System.Math.PI

// Expose OpenTK's Key enum as our own
type Key = OpenTK.Input.Key

module Keyboard = 
    let IsDown(key : Input.Key) = Input.Keyboard.GetState().IsKeyDown(key)
    let IsAnyDown(keys : Input.Key list) = keys |> List.tryFind IsDown |> Option.isSome

type Canvas() = 
    
    member this.resetTransform() = 
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadIdentity()
    
    member this.clear() = 
        GL.ClearColor(0.f, 0.f, 0.2f, 1.f)
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
    
    member this.save() = GL.PushMatrix()
    member this.restore() = GL.PopMatrix()
    member this.translate (x : float, y : float) = GL.Translate(x, y, 0.)
    member this.rotate angle = GL.Rotate(radToDeg angle, Vector3d.UnitZ)
    member this.color (r:float) g b = GL.Color3(r, g, b)
    member this.drawVertices (verts : (float * float) list) =
        GL.Begin(BeginMode.LineLoop)
        verts |> Seq.iter GL.Vertex2
        GL.End()

type DebugDraw() =
    let mutable lines = [] // list of lists of float * float
    member this.line (pos1:float*float) (pos2:float*float) =
        lines <- [pos1; pos2] :: lines
        
    member this.draw (canvas:Canvas) =
        canvas.color 1. 0. 0.
        lines |> List.iter canvas.drawVertices

    member this.clear() =
        lines <- []

let debugDraw = DebugDraw ()

type GameWindow(title, viewportSize) = 
    inherit OpenTK.GameWindow()
    let canvas = Canvas() //@TODO: move to Application
    let mutable onUpdate = fun dt c -> ()
    
    do 
        base.VSync <- VSyncMode.On
        base.Title <- title
    
    member this.setOnUpdate onUpdate' = onUpdate <- onUpdate'
    
    override this.OnLoad e = 
        base.OnLoad(e)
        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        GL.Enable(EnableCap.DepthTest)
    
    override this.OnUpdateFrame e = 
        base.OnUpdateFrame e
        if Input.Keyboard.GetState().[Input.Key.Escape] then base.Close()
    
    override this.OnRenderFrame e = 
        base.OnRenderFrame e
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadIdentity()
        onUpdate e.Time canvas
        debugDraw.draw canvas
        base.SwapBuffers()
    
    override this.OnResize e = 
        base.OnResize e
        let vw, vh = float32 (fst viewportSize), float32 (snd viewportSize)
        let mutable projection = Matrix4.CreateOrthographic(vw, vh, -100.f, 100.f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)
        // Maximize and center viewport to client area while maintain aspect ratio
        let clientRect = base.ClientRectangle
        let cw, ch = float32 (clientRect.Width - clientRect.X), float32 (clientRect.Height - clientRect.Y)
        let mutable vx, vy = 0.f, 0.f
        let mutable vw, vh = vw, vh
        let ar = vw / vh
        vw <- ch * ar
        vh <- ch
        if vw > cw then 
            vw <- cw
            vh <- cw / ar
            vy <- (ch - vh) / 2.f
        else vx <- (cw - vw) / 2.f
        GL.Viewport(int vx, int vy, int vw, int vh)
        // Scissor to viewport so that we see black borders outside of viewport area
        GL.Enable(EnableCap.ScissorTest)
        GL.Scissor(int vx, int vy, int vw, int vh)

let setGameWindowSize width height (gameWindow : GameWindow) = 
    gameWindow.Width <- width
    gameWindow.Height <- height
    gameWindow

let setGameWindowSizeScreenRatio ratio (gameWindow : GameWindow) = 
    let screen = DisplayDevice.Default.Bounds
    setGameWindowSize (int (float screen.Width * ratio)) (int (float screen.Height * ratio)) gameWindow

let centerGameWindow (gameWindow : GameWindow) = 
    let screen = DisplayDevice.Default.Bounds
    gameWindow.X <- (screen.Width - gameWindow.Width) / 2
    gameWindow.Y <- (screen.Height - gameWindow.Height) / 2
    gameWindow

type Application(title, viewportSize) = 
    
    let gameWindow = 
        new GameWindow(title, viewportSize)
        |> setGameWindowSizeScreenRatio 0.8
        |> centerGameWindow
    
    member this.setOnUpdate = gameWindow.setOnUpdate
    member this.run() = gameWindow.Run(30.)
