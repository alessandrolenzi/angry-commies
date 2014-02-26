module AnimationObjects

open System
open System.Drawing

open Utilities

let bigStar = imageFromName "bigstar.png"
//let littleStar = imageFromName "littlestar.png"
let targetHat = imageFromName "target_hat.png"


type Animation(px : float, py : float, sx : float, sy: float, s : float) =
    let mutable posX = px
    let mutable posY = py
    let mutable speedx = sx
    let mutable speedy = sy
    let mutable size = s (*In percentuale! 1 = grande quanto la bitmap*)

    abstract Draw: Graphics -> unit
    abstract Move: float -> unit

    default this.Draw(g:Graphics) =
        if size < 1. then
            use bitmap = new Bitmap(int(float bigStar.Width * s),int(float bigStar.Height * s))
            let h = Graphics.FromImage(bitmap)
            h.DrawImage(bigStar, new Rectangle(0, 0, bitmap.Width, bitmap.Height))
            h.Dispose()
            g.DrawImage(bitmap, new PointF(float32 posX,float32 posY))
            bitmap.Dispose()
        else
            g.DrawImage(bigStar, new PointF(float32 posX, float32 posY))
            
    default this.Move(t : float) =
        posX <- posX + speedx * t/1000.
        posY <- posY + speedy * t/1000.

        speedy <- speedy + 1600. * t/1000.

    member this.PosY with get() = posY

    new(posX : float, posY: float) =
        Animation(posX - float bigStar.Width /2., posY - float bigStar.Height / 2., 0., -800., 1.)

    new(posX: float, posY: float, s: string) =
        if s = "right" then Animation(posX - float bigStar.Width /2., posY - float bigStar.Height / 2., 400., -800., 1.)
        else Animation(posX - float bigStar.Width /2., posY - float bigStar.Height / 2., -400., -800., 1.)


type AnimatedPoints(posX:float, posY:float, p:int) = 
    inherit Animation(posX, posY)
    let points = pointsToImage2 p
    let mutable speedY = -1200.
    let mutable px = posX
    let mutable py = posY

    default this.Draw(g:Graphics) =
        (*Otteniamo i punti!*)
        g.DrawImage(points, new PointF(float32 px,float32 py))
    default this.Move(t:float) = 
        py <- py + speedY * t/1000.

        speedY <- speedY + 1600.*t/1000.


type Hat(posX:float, posY: float) =
    inherit Animation(posX, posY)
    let mutable px = posX - float targetHat.Width/2.
    let mutable py = posY - float targetHat.Height/2.
    let mutable speedy = -500.
    let mutable rotAngle = 0.f

    default this.Draw(g:Graphics) =
        let rotatedHat = rotateImage(targetHat, rotAngle, new PointF(15.f, 15.f))
        rotatedHat.RotateFlip(RotateFlipType.RotateNoneFlipY)
        g.DrawImage(rotatedHat, new PointF(float32 px, float32 py))
        rotatedHat.Dispose()
    
    default this.Move(t:float) =
        py <- py + speedy * t/1000.

        speedy <- speedy + 1600.*t/1000.

        rotAngle <- rotAngle+2.f
        




type Points(start:int, speed:int) =
    let mutable actualPoints = start
    let mutable threesold = start
    new() = Points(0, 5)

    member this.Update(pointsAdded:int) =
        threesold <- threesold + pointsAdded

    member this.Draw(g:Graphics, p: Point) =
        if actualPoints + speed <= threesold  then 
            actualPoints <- actualPoints + speed

        let points = pointsToImage actualPoints
        g.DrawImage(points, p)
        points.Dispose()

    member this.Draw(g:Graphics) = this.Draw(g, new Point(0, 0))

    member this.Reset() = actualPoints <- start; threesold <- start

    member this.Points with get() = threesold  