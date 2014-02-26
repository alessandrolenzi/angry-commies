module PhysicalObjects

open System
open System.Drawing
open System.Windows.Forms

open fsphys
open fsphys.geom
open Utilities
(*
(*Caricamento immagine 1: legno non danneggiato*)
let img1 = sprintf "C:\Users\%s\wood.jpg" System.Environment.UserName
let im = Image.FromFile(img1)
let bitmap1 = new Bitmap(int im.Width, int im.Height)
let g = Graphics.FromImage(bitmap1)
g.DrawImage(im, new RectangleF(0.f, 0.f, float32(bitmap1.Width), float32(bitmap1.Height))) 
im.Dispose()
g.Dispose()
let woodTexture = new TextureBrush(bitmap1)

(*Immagine 2: legno dannegiato*)
let img2 = sprintf "C:\Users\%s\wood_destroyed.png" System.Environment.UserName
let img2H = Image.FromFile(img2)
let bitmap2 = new Bitmap(int img2H.Width, int img2H.Height)
let h = Graphics.FromImage(bitmap2)
h.DrawImage(img2H, new RectangleF(0.f, 0.f, float32 img2H.Width, float32 img2H.Width))
img2H.Dispose()
let damagedWoodTexture = new TextureBrush(bitmap2) *)

let woodLittle = imageFromName "woodLittle.png"
let damagedWoodLittle = imageFromName "woodDamagedLittle.png"
let woodLittleTexture = new TextureBrush(woodLittle)
let damagedLittleWoodTexture = new TextureBrush(damagedWoodLittle)
woodLittle.Dispose()
damagedWoodLittle.Dispose()

let woodLong = imageFromName "woodLong.png"
let damagedLongWood = imageFromName "woodDamagedLong.png"
let woodLongTexture = new TextureBrush(woodLong)
let damagedWoodLongTexture = new TextureBrush(damagedLongWood)
woodLong.Dispose()
damagedLongWood.Dispose()

let woodVeryLong = imageFromName "woodVeryLong.png"
let damagedVeryLongWood = imageFromName "woodDamagedVeryLong.png"
let woodVeryLongTexture = new TextureBrush(woodVeryLong)
let damagedWoodVeryLongTexture = new TextureBrush(damagedVeryLongWood)
woodVeryLong.Dispose()
damagedVeryLongWood.Dispose()

let woodCircle = imageFromName "woodCircle.png"
let damagedCircleWood = imageFromName "woodDamagedCircle.png"
let woodCircleTexture = new TextureBrush(woodCircle)
let damagedWoodCircleTexture = new TextureBrush(damagedCircleWood)
woodCircle.Dispose()
damagedCircleWood.Dispose()

let glassLittle = imageFromName "glassLittle.png"
let damagedLittleGlass = imageFromName "glassDamagedLittle.png"
let glassLittleTexture = new TextureBrush(glassLittle)
let damagedGlassLittleTexture = new TextureBrush(damagedLittleGlass)
glassLittle.Dispose()
damagedLittleGlass.Dispose()

let glassLong = imageFromName "glassLong.png"
let damagedLongGlass = imageFromName "glassDamagedLong.png"
let glassLongTexture = new TextureBrush(glassLong)
let damagedGlassLongTexture = new TextureBrush(damagedLongGlass)
glassLong.Dispose()
damagedLongGlass.Dispose()

let gr = imageFromName "glassVeryLong.png"
let grd = imageFromName "glassVeryLongDamaged.png"
let glassVeryLongTexture = new TextureBrush(gr)
let damagedGlassVeryLongTexture = new TextureBrush(grd)
gr.Dispose()
grd.Dispose()

let glassCircle = imageFromName "glassCircle.png"
let damagedCircleGlass = imageFromName "glassCircleDamaged.png"
let glassCircleTexture = new TextureBrush(glassCircle)
let damagedGlassCircleTexture = new TextureBrush(damagedCircleGlass)
glassCircle.Dispose()
damagedCircleGlass.Dispose()


let RadianToDegree angle = 
   float32 angle * (180.0f / float32 Math.PI);


type [<AbstractClass>]Bullet(s: phys.def.shape, d : scalar) =
    let shape : phys.def.shape = s;
    let density : scalar = d;  
    

    member this.Shape with get() = shape
    member this.Density with get() = density
    abstract DrawInUse: Graphics * PointF -> unit
    abstract DrawUsed: Graphics * PointF -> unit
    abstract DrawHighlight: Graphics * PointF -> unit
    abstract Type : int
    
type IGameLand =
    abstract Shape : phys.def.shape
    abstract Position : vec
    abstract Width : float
    abstract Height : float

type Floor(width:float, height:float, pos: vec) = 
    let box w h =
        let w = w/2.
        let h = h/2.
        phys.def.poly [| vec(w,h); vec(w,-h); vec(-w,-h); vec(-w,h) |]
    let shape =  box width height

    interface IGameLand with
       member this.Shape with get() = shape
       member this.Position with get() = vec(pos.x + width/2., pos.y + height/2.)
       member this.Width = width
       member this.Height = height

type IGameElement =
     
     abstract Shape : phys.def.shape
     abstract Density : scalar
     abstract Resistance : scalar
     abstract Position : vec
     abstract Width: float
     abstract Height : float

     abstract Hit : contact.t -> unit
     abstract Hit : float -> unit
     abstract Hits : unit
     abstract Destroy : bool


type [<AbstractClass>]Obstacle(s: phys.def.shape,w:float, h:float, p:vec, d:scalar, r:scalar, texture:Brush, damagedTexture:Brush) =
    let shape = s
    let density = d
    let mutable resistance = r
    let position = p
    let mutable maxHits = 2
    
    interface IGameElement with
        member this.Shape with get() = shape
        member this.Density with get() = density
        member this.Resistance with get() = resistance
        member this.Position with get() = position
        member this.Hit(c:contact.t) = resistance <- resistance - Math.Abs(c.jnacc) - Math.Abs(c.jtacc)
        member this.Hit(f:float) = resistance <- resistance - f
        member this.Hits = maxHits <- maxHits - 1
        member this.Destroy = resistance <= 0. || maxHits <= 0
        member this.Height = h
        member this.Width = w
            (*Funzione di disegno...!*)
            
    end
    
    abstract Kind: int
    abstract Draw: Graphics*body.t -> unit
    abstract Points:int

    default t.Kind with get() = 1

    default this.Draw(g:Graphics, b:body.t) =
        let texture = if (this :> IGameElement).Resistance < r/2. then damagedTexture else texture
        
        let s : shape.t = b.shapes.[0]
        match s with
            | shape.Poly p -> 
                let m = p.verts.GetEnumerator()
                m.MoveNext() |> ignore
                let v = m.Current :?> vec
                (texture :?> TextureBrush).TranslateTransform(float32 v.x,float32 v.y, Drawing2D.MatrixOrder.Append)
                (texture :?> TextureBrush).RotateTransform(RadianToDegree b.ang)
                g.FillPolygon(texture, [| for v in p.verts -> pointf v |])
                              
            | shape.Circle c ->
                let r = vec(c.radius,c.radius)
                (texture :?> TextureBrush).TranslateTransform(float32 b.pos.x + float32 c.radius, float32 b.pos.y+float32 c.radius)
                g.FillRectangle(texture, new Rectangle(int(c.center.x - c.radius), int(c.center.y - c.radius),int(c.radius)*2, int c.radius * 2)) 

        (texture :?> TextureBrush).ResetTransform()
        

    new(s:phys.def.shape,w:float,h:float,p:vec, d:scalar, r:scalar) =
        Obstacle(s, w, h, p, d, r, woodVeryLongTexture, damagedWoodVeryLongTexture)


type LittleWoodObstacle(p:vec) =
    inherit Obstacle(Utilities.box 50. 50., 50., 50., p, 0.5, 100.,woodLittleTexture, damagedLittleWoodTexture)

    override this.Kind = 2
    
    default this.Points = 20

type LongWoodObstacle(p:vec) =
    inherit Obstacle(Utilities.box 172. 50., 172., 50., p, 0.5, 100., woodLongTexture, damagedWoodLongTexture)

    override this.Kind = 3
    default this.Points = 30

type VeryLongWoodObstacle(p:vec) =
    inherit Obstacle(Utilities.box 400. 50., 400., 50., p, 0.5, 100., woodVeryLongTexture, damagedWoodVeryLongTexture)

    override this.Kind = 4
    default this.Points = 50

type CircleWoodObstacle(p:vec) =
    inherit Obstacle(phys.def.Circle((vec(0., 0.)), 30.), 60., 60., p, 0.5, 100., woodCircleTexture, damagedWoodCircleTexture)

    override this.Kind = 5
    default this.Points = 20

type LittleGlassObstacle(p:vec) =
    inherit Obstacle(Utilities.box 50. 50., 50., 50., p, 2.5, 10., glassLittleTexture, damagedGlassLittleTexture)

    override this.Kind = 6

    default this.Points = 5

type LongGlassObstacle(p:vec) =
    inherit Obstacle(Utilities.box 173. 50., 173., 50., p, 2.5, 10., glassLongTexture, damagedGlassLongTexture)

    override this.Kind = 7
    
    default this.Points = 10

type VeryLongGlassObstacle(p:vec) =
    inherit Obstacle(Utilities.box 400. 50., 400., 50., p, 2.5, 10., glassVeryLongTexture, damagedGlassVeryLongTexture)

    override this.Kind = 8
    
    default this.Points = 20

type CircleGlassObstacle(p:vec) =
    inherit Obstacle(phys.def.Circle((vec(0., 0.)), 30.), 60., 60., p, 2.5, 10., glassCircleTexture, damagedGlassCircleTexture)

    override this.Kind = 9
    default this.Points = 5


    

(*     
type GlassObstacle(s:phys.def.shape, w:float, h:float, p:vec)=
    inherit Obstacle(s,w,)

    do
        base.Kind <- 1
    
    default this.Points with get() = 10

    default this.Draw(g: Graphics, b:body.t) = 
        let f = new Font(FontFamily.GenericSerif, 12.f)
        let texture = if (this :> IGameElement).Resistance < 2. then damagedGlassTexture else glassTexture
        let s : shape.t = b.shapes.[0]
        match s with
            | shape.Poly p -> g.FillPolygon(texture, [| for v in p.verts -> pointf v |])
                                                           
            | shape.Circle c ->
                let r = vec(c.radius,c.radius)
                g.FillEllipse(texture,
                    RectangleF(pointf (c.center -| r), SizeF(pointf (2. .* r)))) 
        

        texture.ResetTransform()
*)
let gl = imageFromName "target1.png"
let general = new TextureBrush(gl)
let g2 = imageFromName "target2.png"
let general2 = new TextureBrush(g2)
g2.Dispose()
let g3 = imageFromName "target3.png"
let general3 = new TextureBrush(g3)
g3.Dispose()
let g4 = imageFromName "target4.png"
let general4 = new TextureBrush(g4)
g4.Dispose()
let g5 = imageFromName "target5.png"
let general5 = new TextureBrush(g5)
g5.Dispose()
let g6 = imageFromName "target6.png"
let general6 = new TextureBrush(g6)
g6.Dispose()

type Target(s:phys.def.shape, p:vec)=
    inherit Obstacle(s, float gl.Width, float gl.Height, p, 5., 10., woodVeryLongTexture, damagedWoodVeryLongTexture)

    let mutable animTime = 0.
    let totalAnimation = 2500.

    new(p:vec) =
        Target(Utilities.box 60. 78., p)

    default this.Points with get() = 100

    default this.Draw(g:Graphics, b:body.t) = 
        let brush = if animTime < 500. then general
                    else 
                        if animTime < 1000. then general2
                        else 
                            if animTime < 1500. then general3
                            else if animTime < 1750. then general4
                                 else if animTime < 2000. then general5 
                                 else general6
        let s : shape.t = b.shapes.[0]
        match s with
            | shape.Poly p -> 
                let m = p.verts.GetEnumerator()
                m.MoveNext() |> ignore
                let v = m.Current :?> vec
                brush.TranslateTransform(float32 v.x + 1.f,float32 v.y, Drawing2D.MatrixOrder.Append)
                brush.RotateTransform(RadianToDegree b.ang)
                g.FillPolygon(brush, [| for v in p.verts -> pointf v |])
                brush.ResetTransform()
            | _ -> ()

        //g.DrawImage(img, new PointF(float32 b.pos.x - 25.f, float32 b.pos.y - 37.5f))
      
    member this.Move(t:float) = (*tempo in millisecondi*)
        animTime <- (animTime + t + float(ri(50))) % totalAnimation

    default this.Kind with get() = 9

let normalBullet = imageFromName "redbullet.png"
let explosiveBullet1 = imageFromName "explosivebullet1.png"
let explosiveBullet2 = imageFromName "explosivebullet2.png"
let usedBullet = imageFromName "used.png"
let highlight = imageFromName "highlight.png"
let highlightBomb = imageFromName "highlightBomb.png"
let explosion = imageFromName "explosion.png"

type NormalBullet() =
    inherit Bullet(phys.def.Circle((vec(0., 0.)), 20.), 50.)

    default this.DrawInUse(g:Graphics, point:PointF) =
        g.DrawImage(normalBullet, new PointF(point.X - 22.f, point.Y - 22.f))
       
    
    default this.DrawUsed(g: Graphics, point: PointF) = 
       this.DrawInUse(g, point)
       g.DrawImage(usedBullet, new PointF(point.X - 22.f, point.Y - 22.f))

    default this.DrawHighlight(g: Graphics, point:PointF) =
        g.DrawImage(highlight, new PointF(point.X - float32 highlight.Width/2.f, point.Y - float32 highlight.Height / 2.f))

    default this.Type with get() = 0
type ExplosiveBullet() =
    inherit Bullet(phys.def.Circle((vec(0., 0.)), 20.), 50.)
    let mutable odd = true
    let mutable exploded = false
    let mutable explosionEnded = false
    let mutable explosionArea = new RectangleF()
    let threesold = 300.f
    let mutable explosionPoint = new PointF()
    let mutable i = 0

    default this.DrawInUse(g:Graphics, point:PointF) =
        if exploded && not(explosionEnded) then 
            explosionArea <- new RectangleF(explosionArea.X-8.f, explosionArea.Y-8.f,explosionArea.Width + 16.f, explosionArea.Height + 16.f)
            g.DrawImage(explosion, explosionArea)
        else
            if odd then
                g.DrawImage(explosiveBullet1, new PointF(point.X - float32 explosiveBullet1.Width/2.f, point.Y - float32 explosiveBullet1.Height/2.f  - 6.f))
            else
                g.DrawImage(explosiveBullet2, new PointF(point.X - float32 explosiveBullet2.Width/2.f, point.Y - float32 explosiveBullet2.Height/2.f  - 6.f))
            if i = 0 then odd <- not(odd)
            i <- (i + 1) % 4
        if explosionArea.Width > threesold then explosionEnded <- true
       

    default this.DrawUsed(g: Graphics, point: PointF) = 
       this.DrawInUse(g, point)
       g.DrawImage(usedBullet, new PointF(point.X - 20.f, point.Y - 22.f))

    default this.DrawHighlight(g: Graphics, point:PointF) =
        g.DrawImage(highlightBomb, new PointF(point.X - float32 highlightBomb.Width/2.f , point.Y - float32 highlightBomb.Height / 2.f - 3.f))

    default this.Type with get() = 1

    member this.Exploded with get() = exploded

    member this.StartExplosion(p:PointF) = (*p sarà la posizione in cui si trova il corpo nel momento in cui inizia l'esplosione*)
        explosionArea <- new RectangleF(p.X - 50.f, p.Y - 50.f, 100.f, 100.f)
        exploded <- true
        explosionPoint <- p
                                
    member this.ExplosionEnded with get() = explosionEnded 

    member this.ExplosionArea with get() = explosionArea

    member this.ExplosionPoint with get() = new vec(float explosionPoint.X, float explosionPoint.Y)