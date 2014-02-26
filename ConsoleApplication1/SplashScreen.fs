module SplashScreen

open System
open System.Drawing
open System.Windows.Forms

open Utilities


type SplashScreen(t: int, p:int) as this=
    inherit UserControl()
    let img = imageFromName(sprintf "splash%d.png" t)
    let points = pointsToImage p
    let restartRequired = new Event<unit>()
    let restart = imageFromName "restart.png"
    let mutable restartRect = new Rectangle(this.ClientRectangle.Width / 2 - img.Width/2 + 20, this.ClientRectangle.Height / 2 + 50, restart.Width, restart.Height)
    let mutable mouseDownRestart = false
    do
        this.Dock <- DockStyle.Fill

    member this.RestartRequired = restartRequired.Publish

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(skytexture, new Rectangle(0, 0, this.ClientRectangle.Width, 349))
        hillstexture.TranslateTransform(0.f,float32 600 - 249.f)
        g.FillRectangle(hillstexture, new Rectangle(0,int 615 - 249, this.ClientRectangle.Width, 249))
        hillstexture.ResetTransform()
        g.FillRectangle(skyobjecttexture, new Rectangle(0, 0, this.ClientRectangle.Width, 482))
        dirttexture.TranslateTransform(0.f,float32 615 - 25.f)
        g.FillRectangle(dirttexture, new Rectangle(0, int 615 - 25,  this.ClientRectangle.Width, this.ClientRectangle.Height - (615 - 30)))
        dirttexture.ResetTransform()
        let b = new SolidBrush(Color.FromArgb(100, 150, 150, 150))
        e.Graphics.FillRectangle(b, this.ClientRectangle)
        e.Graphics.DrawImage(img, new Point(this.ClientRectangle.Width / 2 - img.Width/2, this.ClientRectangle.Height / 2 - img.Height/2))
        e.Graphics.DrawImage(points, new Point(this.ClientRectangle.Width /2, this.ClientRectangle.Height / 2 - img.Height/2 + 140))
        b.Dispose()
        
        g.DrawImage(restart, restartRect)

    override this.OnMouseDown(e) = 
        if restartRect.Contains(e.Location) then mouseDownRestart <- true
        else mouseDownRestart <- false
    override this.OnMouseUp(e) =
        if mouseDownRestart && restartRect.Contains(e.Location)  then
            restartRequired.Trigger()
        else
            mouseDownRestart <- false
    override this.OnResize(e) =
        restartRect <- new Rectangle(this.ClientRectangle.Width / 2 - img.Width/2 + 20, this.ClientRectangle.Height / 2 + 50, restart.Width, restart.Height)
   