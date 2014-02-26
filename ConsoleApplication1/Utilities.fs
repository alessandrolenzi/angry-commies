module Utilities

open System
open System.Drawing

open fsphys
open fsphys.geom

let dottedPen = new Pen(Color.Red, 3.f)
dottedPen.DashStyle <- Drawing2D.DashStyle.Dash

let controlBrush = new SolidBrush(Color.FromArgb(100, 150, 150, 150))
let hoverBrush = new SolidBrush(Color.FromArgb(150, 100, 100, 100))
let controlFont = new Font(FontFamily.GenericSerif, 12.f)
let bigFont = new Font(FontFamily.GenericSansSerif, 20.f)
let wrongBrush = new SolidBrush(Color.FromArgb(150, 200, 0, 0))


let pointf (v:vec) = PointF(float32 v.x, float32 v.y)

let box w h =
  let w = w/2.
  let h = h/2.
  phys.def.poly [| vec(w,h); vec(w,-h); vec(-w,-h); vec(-w,h) |]


let imageFromName (a:string) = 
    let img = sprintf "C:\Users\%s\%s" System.Environment.UserName a
    let im = Image.FromFile(img)
    let bitmap = new Bitmap(int im.Width, int im.Height)
    let g = Graphics.FromImage(bitmap)
    g.DrawImage(im, new RectangleF(0.f, 0.f, float32(bitmap.Width), float32(bitmap.Height))) 
    im.Dispose()
    g.Dispose()

    bitmap

let rotateImage (b:Bitmap, angle:float32, rotP : PointF) =
    let pts : Point[]= [| new Point(0, 0); new Point(b.Width, 0); new Point(b.Width, b.Height); new Point(0, b.Height)|]
    let ptsEnum = pts.GetEnumerator()
    while ptsEnum.MoveNext() do
        let mutable p = ptsEnum.Current :?> Point
        p.X <- p.X - b.Width/2
        p.Y <- p.Y - b.Height/2
    done
    ptsEnum.Reset()
    let m = new System.Drawing.Drawing2D.Matrix()
    m.RotateAt(angle, rotP)
    m.TransformPoints(pts)
    m.Dispose()
    let mutable maxX = -56635
    let mutable maxY = -56635
    let mutable minX = 56635
    let mutable minY = 56635
    while ptsEnum.MoveNext() do
        let p = ptsEnum.Current :?> Point
        if maxX < p.X then maxX <- p.X
        if minX > p.X then minX <- p.X

        if maxY < p.Y then maxY <- p.Y
        if minY > p.Y then minY <- p.Y
    done
    ptsEnum.Reset()
    while ptsEnum.MoveNext() do 
        let mutable p = ptsEnum.Current :?> Point
        p.X <- p.X - minX
        p.Y <- p.Y - minY
    done
    let retbmp = new Bitmap(maxX - minX, maxY - minY);
    let g = Graphics.FromImage(retbmp)
    ptsEnum.Reset()
    let finalPts = 
        [| 
            for i = 0 to 3 do 
                ptsEnum.MoveNext() |> ignore
                if i <> 2 then
                    yield ptsEnum.Current :?> Point
            done
        |]
    g.DrawImage(b, finalPts)
    g.Dispose()
    
    retbmp

let figures_1 = new ResizeArray<Bitmap>()
for i = 0 to 9 do
    figures_1.Add(imageFromName(sprintf "number%d.png" i))
done

let figures_2 = new ResizeArray<Bitmap>()
for i = 0 to 9 do
    figures_2.Add(imageFromName(sprintf "number2%d.png" i))
done

let pointsToImage(points:int) =
    let str = sprintf "%d" points
    let length = str.Length
    let width = str.Length * 36
    let bitmap = new Bitmap(width, 50)
    let mutable i = 0
    let g = Graphics.FromImage(bitmap)
    while i < length do
        let c = str.Chars(i)
        let figure = match c with
                        | '0' -> 0
                        | '1' -> 1
                        | '2' -> 2
                        | '3' -> 3
                        | '4' -> 4
                        | '5' -> 5
                        | '6' -> 6
                        | '7' -> 7
                        | '8' -> 8
                        | '9' -> 9
        
        g.DrawImage(figures_1.[figure], new Point(36*i, 0))
        i <- i + 1
    done
    g.Dispose()

    bitmap

let resizeImage(b:Bitmap, w:int, h:int) =
    let mutable newWidth, newHeight = 0, 0
    let ratio = b.Width / b.Height
    newWidth <- Math.Min(b.Width, w) 
    newHeight <- newWidth/ratio
    let resized = new Bitmap(newWidth, newHeight)
    let h = Graphics.FromImage(resized)
    h.DrawImage(b, new RectangleF(0.f, 0.f, float32 newWidth, float32 newHeight))
    h.Dispose()

    resized


let pointsToImage2(points:int) =
    let str = sprintf "%d" points
    let length = str.Length
    let width = str.Length * 18
    let bitmap = new Bitmap(width, 24)
    let mutable i = 0
    let g = Graphics.FromImage(bitmap)
    while i < length do
        let c = str.Chars(i)
        let figure = match c with
                        | '0' -> 0
                        | '1' -> 1
                        | '2' -> 2
                        | '3' -> 3
                        | '4' -> 4
                        | '5' -> 5
                        | '6' -> 6
                        | '7' -> 7
                        | '8' -> 8
                        | '9' -> 9
        
        g.DrawImage(figures_2.[figure], new Point(18*i, 0))
        i <- i + 1
    done
    g.Dispose()

    bitmap

let ri,rd =
  let r = System.Random()
  r.Next, r.NextDouble


let sign(p1 : PointF, p2: PointF, p3:PointF) =
  (p1.X - p3.X) * (p2.Y - p3.Y) - (p2.X - p3.X) * (p1.Y - p3.Y)

let pointInTriangle(pt:PointF, v1:PointF, v2:PointF, v3:PointF) =
  let b1 = sign(pt, v1, v2) < 0.0f
  let b2 = sign(pt, v2, v3) < 0.0f
  let b3 = sign(pt, v3, v1) < 0.0f

  ((b1 = b2) && (b2 = b3))

let pointInRectangle(pt:PointF, p: vec array) =
    let enum = p.GetEnumerator()
    enum.MoveNext() |> ignore
    let a = pointf (enum.Current :?> vec)
    enum.MoveNext() |> ignore
    let b = pointf (enum.Current :?> vec)
    enum.MoveNext() |> ignore
    let c = pointf (enum.Current :?> vec)
    enum.MoveNext() |> ignore
    let d = pointf (enum.Current :?> vec)
    
    pointInTriangle(pt, a, b, c) || pointInTriangle(pt, c, d , a)





let bitmap0 = imageFromName "hills.png"
let bitmap1 = imageFromName "skytexture.png"
let bitmap2 = imageFromName "dirt.png"
let bitmap3 = imageFromName "grass.png"
let bitmap4 = imageFromName "skyobjects.png"
let bitmap5 = imageFromName "redbullet.png"

let skytexture = new TextureBrush(bitmap1)
bitmap1.Dispose()
let dirttexture = new TextureBrush(bitmap2)
bitmap2.Dispose()
let grasstexture = new TextureBrush(bitmap3)
bitmap3.Dispose()
let skyobjecttexture = new TextureBrush(bitmap4)
bitmap4.Dispose()
let hillstexture = new TextureBrush(bitmap0)
bitmap0.Dispose()

