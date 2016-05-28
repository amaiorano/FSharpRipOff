#if !INTERACTIVE
module OpenTKPlatform
#endif

#if INTERACTIVE
#r "../external/OpenTK/OpenTK.dll"
#endif

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

let degToRad (degs:float) = degs * System.Math.PI / 180.0
let radToDeg (rads:float) = rads * 180.0 / System.Math.PI

// Expose OpenTK's Key enum as our own
type Key = OpenTK.Input.Key

module Keyboard =
    let IsDown (key:Input.Key) = Input.Keyboard.GetState().IsKeyDown(key)

type Canvas() =
    member this.resetTransform () =
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadIdentity()
    member this.clear() = 
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
    member this.save () = GL.PushMatrix()
    member this.restore () = GL.PopMatrix()
    member this.translate (x:float, y:float) = GL.Translate(x,y,0.)
    member this.rotate angle = GL.Rotate(radToDeg angle, Vector3d.UnitZ)
    member this.drawVertices (verts:(float*float) list) =
        GL.Begin(BeginMode.LineLoop)
        GL.Color3(1.f, 1.f, 1.f);
        verts |> Seq.iter GL.Vertex2
        GL.End()

type GameWindow(title) =
    inherit OpenTK.GameWindow()
    let canvas = Canvas() //@TODO: move to Application
    let mutable onUpdate = fun dt c -> ()
    do
        base.VSync <- VSyncMode.On
        base.Title <- title

    member this.setOnUpdate onUpdate' = onUpdate <- onUpdate'
            
    override this.OnLoad e =
        base.OnLoad(e);
        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        GL.Enable(EnableCap.DepthTest)

    override this.OnUpdateFrame e =
        base.OnUpdateFrame e
        if Input.Keyboard.GetState().[Input.Key.Escape] then base.Close()
//        onUpdate e.Time canvas

    override this.OnRenderFrame e =
        base.OnRenderFrame e
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadIdentity()
        onUpdate e.Time canvas
        base.SwapBuffers()

    override this.OnResize e =
        base.OnResize e
        let rect = base.ClientRectangle
        GL.Viewport(rect.X, rect.Y, rect.Width, rect.Height)
        let mutable projection = Matrix4.CreateOrthographicOffCenter(0.f, float32 rect.Width, float32 rect.Height, 0.f, -100.f, 100.f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)

let setGameWindowSize width height (gameWindow:GameWindow) =
    gameWindow.Width <- width
    gameWindow.Height <- height
    gameWindow

let setGameWindowSizeScreenRatio ratio (gameWindow:GameWindow) =
    let screen = DisplayDevice.Default.Bounds
    setGameWindowSize (int (float screen.Width * ratio)) (int (float screen.Height * ratio)) gameWindow

let centerGameWindow (gameWindow:GameWindow) =
    let screen = DisplayDevice.Default.Bounds
    gameWindow.X <- (screen.Width - gameWindow.Width) / 2
    gameWindow.Y <- (screen.Height - gameWindow.Height) / 2
    gameWindow

type Application(title) =
    let gameWindow =
        new GameWindow(title)
            |> setGameWindowSizeScreenRatio 0.8
            |> centerGameWindow
    member this.setOnUpdate = gameWindow.setOnUpdate
    member this.run () = gameWindow.Run(30.)
