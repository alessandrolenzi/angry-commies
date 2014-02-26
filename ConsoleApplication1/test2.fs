module test

open System.Windows.Forms
open System.Drawing


open System.Collections.Generic

open fsphys
open fsphys.geom

type StaticObject(s: vec array, initialPos : vec) as this = 
    let sh : phys.def.shape = phys.def.Poly(s)
    let obj:body.t = body.create (vec(0., 0.)) infinity [shape.poly s] initialPos 0.

    member this.pos with get() = obj.pos

(*Oggetti fisici che si possono muovere li vogliamo manovrare facilmente*)
type MovingObject(s : vec array, mass : scalar, initialSpeed : vec, initialPos : vec) as this =
    let sh : phys.def.shape = phys.def.Poly(s)
    let mutable staticObj : phys.def.body = phys.def.body 1. [sh] initialSpeed
    let mutable obj = body.create (vec(0., 0.)) infinity [shape.poly s] (vec(0., 0.)) 0.
    let mutable identifier = 0
    (*Cosa vogliamo che faccia questo oggetto? Un sacco di cose paxxixime
        -- Modifica Velocità
        -- Cancellazione dell'oggetto
    *)

    member this.shape with get() = sh;

    member this.setBody(b : body.t) = obj <- b;

    member this.updateSpeed(speedx : float, speedy: float) = obj.vel <- vec(speedx, speedy);

    member this.updateSpeedX(speedx : float) = obj.vel <- vec(speedx, obj.vel.y);

    member this.updateSpeedY(speedy : float) = obj.vel <- vec(obj.vel.y, speedy);

    member this.speed with get() = obj.vel

    member this.pos with get() = obj.pos

    member this.id with get() = identifier and set(v) = identifier <- v

let mutable msstring = 0;

type PhysicalEnvironment() as this =
    let fps = 20.
    let movingObjects : ResizeArray<MovingObject> = new ResizeArray<MovingObject>()
    let staticObjects : ResizeArray<StaticObject> = new ResizeArray<StaticObject>()
    let ri,rd =
        let r = System.Random()
        r.Next, r.NextDouble
    let mutable p = ref(this.init())
    let box w h =
        let w = w/2.
        let h = h/2.
        phys.def.poly [| vec(w,h); vec(w,-h); vec(-w,-h); vec(-w,h) |]

    let randshape () =
        let n = float (4 + ri 4)
        phys.def.poly
            [| for i in 1. .. n ->
                (30.+rd()*20.) .* vpolar(-i/n*6.28)
            |]

    let circle () =
        phys.def.circle 20.
    do (*comportamento del costruttore *)
        p <- ref(this.init())

        

    member this.poo with get() = p and set(v : phys.main ref) = p <- v 

    member this.update() = (!this.poo).update()

    member this.init () =
      let q = phys.init <| phys.default_cfg 20.
      List.iter (q.add_body>>ignore)
        [ for _ in 0 .. 30 do
            let pos = vec(rd()*700., rd()*200.)
            yield (pos,0.), phys.def.body 1. [randshape ()] (vec(0., 0.))
      
          yield
            (vec(350., 450.),0.), phys.def.body_static [box 700. 50.]
          ]
      q

    member this.draw(g:Graphics) =
        let d = (!p).dump
        let line = new Pen(Brushes.Black)
        let green = Brushes.Green  
        let pointf (v:vec) = PointF(float32 v.x, float32 v.y)  
        g.Clear(Color.White)  
        for b in d.bodies do
        for s in b.shapes do
            match s with
            | shape.Poly p ->
                    g.DrawPolygon(line, 
                        [| for v in p.verts -> pointf v |] )
            | shape.Circle c ->
                let r = vec(c.radius,c.radius)
                g.DrawEllipse(line,RectangleF(pointf (c.center -| r), SizeF(pointf (2. .* r))) )

        for ct in d.contacts.Values do
            let r = let s = 5. in vec(s,s)
            g.FillEllipse(green, RectangleF(pointf (ct.p-|0.5.*r), SizeF(pointf r)) )
        done
    
   


type Main =
  inherit Form
  
  new() as w = {} then
    let sp = new PhysicalEnvironment();
    w.Width <- 700
    w.Height <- 1000
    w.DoubleBuffered <- true
    let l = new Label()
    l.Text <- "Click to reset"
    w.Controls.Add l
    
    let t = new Timer()
    t.Interval <- 1000/int 20
    t.Start()
    w.Closed.Add (fun _ -> t.Stop())
    t.Tick.Add (fun _ -> sp.update(); w.Invalidate())
    //t.Tick.Add(fun _ -> w.Text <- let body0 = sp.physics.contents.dump.bodies.[0] in sprintf "%d %f %f" msstring body0.vel.x body0.vel.y)
    w.Paint.Add (fun e -> try sp.draw e.Graphics with _ -> ())
    
    w.Click.Add (fun _ -> sp.poo := sp.init ();)


do (new Main()).ShowDialog() |> ignore