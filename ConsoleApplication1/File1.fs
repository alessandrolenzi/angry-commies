
module File1

open System
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms

open fsphys
open fsphys.geom
open Utilities
open GameControl
open PhysicalObjects
open SplashScreen
open LevelEditor

let l1 = new Floor(3000., 50., vec(0., 615.))
let landscape = new ResizeArray<IGameLand>()
landscape.Add(l1)

let o0 = new LittleWoodObstacle(vec(1790., 200.));
let o1 = new LongWoodObstacle(vec(1750., 500.));
let o2 = new VeryLongWoodObstacle(vec(700., 500.));
let o3 = new CircleWoodObstacle(vec(900., 500.));

let o4 = new LongGlassObstacle(vec(500., 455.));
let o5 = new LittleGlassObstacle(vec(700., 300.))
let o6 = new VeryLongGlassObstacle(vec(600., 200.))
let o7 = new CircleGlassObstacle(vec(1790., 200.))

let obstacles = [|o0:>IGameElement; o1:>IGameElement;o2:>IGameElement;o3:>IGameElement;o4:>IGameElement;o5:>IGameElement;o6:>IGameElement;o7:>IGameElement|]

let t1 = new Target(Utilities.box 50. 76., vec(1700., 375.))
let t2 = new Target(Utilities.box 50. 76., vec(1900., 275.))
let targets = [|t1:>IGameElement; t2:>IGameElement|]

let bullets = new ResizeArray<Bullet>();
//region.Complement(new RectangleF(0.f, 0.f, float32 b.Width, float32 b.Height))
bullets.Add(new NormalBullet())
bullets.Add(new ExplosiveBullet())
bullets.Add(new ExplosiveBullet())
bullets.Add(new ExplosiveBullet())



type Main(w, h) as this =
  inherit Form(Width = w, Height = h)
  let mutable game = new GameControl(30., landscape, obstacles, targets, bullets)
  let mutable gameResult = -1
  let mutable gamePoints = 0
  let mutable gaming = false
  let mutable closed = false
  let mutable editor = new LevelEditor()
  let RestartRequired =
    new Handler<unit>(
        fun sender eventargs ->
            this.Controls.Clear()
            game.Dispose()
            game <- new GameControl(24., landscape, obstacles, targets, bullets)
            this.Controls.Add(game)
            game.Dock <- DockStyle.Fill
    )
  let GameControl_GameEnded =
        new Handler<int*int>(
            fun sender eventargs ->
                match eventargs with 
                    |(a, b) -> 
                        let s = new SplashScreen(a, b)
                        gameResult <- a; gamePoints <- b; this.Controls.Add(s); this.Controls.SetChildIndex(s, 0)
                        s.RestartRequired.AddHandler(RestartRequired)        
                this.Invalidate()
        )
  
  do
    (*
    game.Dock <- DockStyle.Fill
    this.Controls.Add(game)
    this.MinimumSize <- new Size(1000, 800)
    this.MaximumSize <- new Size(1000, 800)
    game.GameEnded.AddHandler(GameControl_GameEnded) 
    gaming <- true*)
    editor <- new LevelEditor()
    editor.Dock <- DockStyle.Fill
    this.Controls.Add(editor)
    
  member this.Game with get() = game 
  
  override this.OnClosing(e) =
    base.OnClosing(e)
    this.Game.Stop()
    editor.Stop()
    closed <- true
    
  member this.Run() =
    if gaming then game.Run()
    else editor.Run()
    
  member this.CloseClicked with get() = closed

let f = new Main(1000, 800)
f.BackColor <- Color.White
(*
f.KeyDown.Add(fun e ->
    match e.KeyCode with
    | Keys.R -> f.Controls.Clear(); f.Controls.Add(game)
    | _ -> ()
)*)
//f.Paint.Add(fun e -> e.Graphics.DrawImage(PhysicalObjects.bitmap1, new Rectangle(0, 0, 100, 100))) 
do (
    f.Show() |> ignore
    while not f.CloseClicked do
        let startT = System.DateTime.Now
        f.Run()
        Application.DoEvents()
        let endT = System.DateTime.Now
        let interval = endT - startT
        let sleepTime = Math.Max(0, 1000/30 - int interval.TotalMilliseconds)
        if sleepTime > 5 then
            System.Threading.Thread.Sleep(sleepTime)
    done
    f.Dispose()
    ) |> ignore
