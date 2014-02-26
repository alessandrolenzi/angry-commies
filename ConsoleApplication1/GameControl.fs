module GameControl

open System
open System.Drawing
open System.Windows.Forms

open fsphys
open fsphys.geom
open PhysicalObjects
open AnimationObjects
open Utilities



  
let cannon_base = imageFromName("cannon_base.png")
let cannon = imageFromName("cannon.png")
let cannon_over = imageFromName("cannon_over.png")
let cannon_red = imageFromName("cannon_red.png")

let randshape () =
  let n = float (4 + ri 4)
  phys.def.poly
    [| for i in 1. .. n ->
         (30.+rd()*20.) .* vpolar(-i/n*6.28)
       |]

let circle () =
  phys.def.circle 20.



type GameControl(fps:float, landscape: IGameLand ResizeArray, obstacles : IGameElement seq, targets :IGameElement seq, bullets : Bullet ResizeArray) as this =
    inherit UserControl()
    (*Oggetti usati nel gioco*)
    let mutable currentBullet = 0
    
    let mutable started = false

    let mutable points = new Points()

    let mutable skyObjectsMovement = 0.f

    let mutable ended = false
    let mutable result = 0

    let mutable angle = 15.f

    let mutable landscapeBegin = 1
    let mutable landscapeEnd = 2

    let ObjectsArray : ResizeArray<IGameElement> = ResizeArray<IGameElement>();
    let mutable objectEnd = 3

    let TargetsArray : ResizeArray<IGameElement> = ResizeArray<IGameElement>();
    let mutable targetEnd = 4

    let AnimationArray : ResizeArray<Animation> = ResizeArray<Animation>();

    let mutable initialZoom = true
    let mutable zoom : float32 = 0.5f
    let mutable zoomPosX : float32 = 1.f
    let mutable zoomPosY : float32 = 1.f

    let mutable stop = false
   
    let mutable viewAllRect = new Rectangle(this.ClientRectangle.Width - 50, 0, 50, this.ClientRectangle.Height)
    let mutable viewSomeRect = new Rectangle(0, 0, 50, this.ClientRectangle.Height)
    let mutable zoomingOut = false
    let mutable zoomingIn = false

    let mutable loading = false

    let mutable aimingRect = new Rectangle(0,int landscape.[0].Position.y - int landscape.[0].Height/2 - 400, 400, 400)
    let mutable cannonStrength = 0.f
    let mutable isFiring = false

    let mutable explosion = false
    let mutable explosionRect = new Rectangle()

    let gameEnded = new Event<int * int>()

    let load() = 
        ObjectsArray.Clear()
        for obj in obstacles do
            ObjectsArray.Add(obj)
            
        done
        TargetsArray.Clear()
        for tar in targets do
            TargetsArray.Add(tar)
        done
     
    
                
        
    (*Inizializzazioni per il motore fisico*)
    let init () =
          
          load()
          let p = phys.init <| phys.default_cfg fps
  
          List.iter (p.add_body>>ignore)
            [ let pos = vec(20., 425.)
              yield (pos,0.), phys.def.body (bullets.[0].Density) [bullets.[0].Shape] (vec(0., 0.)) (*In posizione iniziale avremo sempre il proiettile. Nuovo proiettile = premi n*)
              for l in landscape do 
                yield (l.Position,0.), phys.def.body_static [l.Shape]
              done
              landscapeEnd <- landscapeBegin + landscape.Count
              for obj in ObjectsArray do
                 yield (obj.Position,0.), phys.def.body obj.Density [obj.Shape] (vec(0., 0.))
              done
              objectEnd <- landscapeEnd + ObjectsArray.Count
                  
              for tar in TargetsArray do
                yield(tar.Position, 0.), phys.def.body tar.Density [tar.Shape] (vec(0., 0.))
              done
              targetEnd <- objectEnd + TargetsArray.Count
              ]
  
          p

    let mutable p = ref (init ())

    let update () = (!p).update()

    let mutable gameTime = 0.f

    (*Controlliamo se tutti i corpi sono fermi!*)
    let checkLoaded() =
        let mutable allStopped = true
        let mutable i = 0
        while i < p.contents.dump.bodies.Count && allStopped do
            let b = p.contents.dump.bodies.[0]
            if b.vel.x <> 0. || b.vel.y <> 0. then
                allStopped <- false
            i <- i + 1
        done

        allStopped

    (*Timer*)
    let t = new Timer(Interval = 1000/int fps);

    (*Gestione dei contatti*)
    let contactKey a b =
        if a < p.contents.dump.bodies.Count && b < p.contents.dump.bodies.Count then
            hash(hash p.contents.dump.bodies.[a].shapes.[0] + hash p.contents.dump.bodies.[b].shapes.[0], 0)
        else
            0
    

    let contactExists a b =
        if a >= p.contents.dump.bodies.Count || b >= p.contents.dump.bodies.Count then false
        else
            let ctid = hash(hash p.contents.dump.bodies.[a].shapes.[0] + hash p.contents.dump.bodies.[b].shapes.[0], 0)
            in p.contents.dump.contacts.ContainsKey(ctid)
    
    let mutable oldContacts : ResizeArray<space.ctid> = ResizeArray<space.ctid>() 

    (*Gestione degli oggetti fisici*)

    let nextBullet() = 
        explosion <- false
        if objectEnd = targetEnd then (*Abbiamo finito gli ostacoli!*)
            this.Stop()
            gameEnded.Trigger(1, points.Points)
           
        else
            if currentBullet = bullets.Count - 1 && objectEnd < targetEnd then (*Game Over!*)
                this.Stop()
                gameEnded.Trigger(0, points.Points)
            else
                currentBullet <- currentBullet + 1
                p.contents.dump.bodies.RemoveAt(0);
                let pos = vec(20., 570.) in
                (!p).add_body((pos,0.), phys.def.body (bullets.[currentBullet].Density) [bullets.[currentBullet].Shape] (vec(0., 0.)))|> ignore
                (!p).update()
                p.contents.dump.bodies.Insert(0, p.contents.dump.bodies.[p.contents.dump.bodies.Count - 1])
                p.contents.dump.bodies.RemoveAt(p.contents.dump.bodies.Count - 1)
                started <- false
        

    let deleteAnnihilated() =
        let mutable s = ObjectsArray.Count
        let mutable i = 0
        while i < s do
            if ObjectsArray.[i].Destroy then (*Eliminiamo l'oggetto*)
                ObjectsArray.RemoveAt(i)
                if i+landscapeEnd < p.contents.dump.bodies.Count then p.contents.dump.bodies.RemoveAt(i+landscapeEnd)
                objectEnd <- objectEnd - 1
                targetEnd <- targetEnd - 1
                points.Update(10)
            else
                i <- i + 1 (*Siccome l'array è ridimensionabile, dobbiamo incrementare SOLO se non rimuoviamo.*)
            s <- ObjectsArray.Count
        s <- TargetsArray.Count
        i <- 0
        while i < s do
            if TargetsArray.[i].Destroy then (*Eliminiamo l'oggetto*)
                
                TargetsArray.RemoveAt(i)
                if i + objectEnd < p.contents.dump.bodies.Count then (*Aggiunta animazioni!*)
                    AnimationArray.Add(new Animation(p.contents.dump.bodies.[i+objectEnd].pos.x, p.contents.dump.bodies.[i+objectEnd].pos.y, "left")) 
                    AnimationArray.Add(new Animation(p.contents.dump.bodies.[i+objectEnd].pos.x, p.contents.dump.bodies.[i+objectEnd].pos.y))
                    AnimationArray.Add(new Animation(p.contents.dump.bodies.[i+objectEnd].pos.x, p.contents.dump.bodies.[i+objectEnd].pos.y, "right")) 
                    AnimationArray.Add(new AnimatedPoints(p.contents.dump.bodies.[i+objectEnd].pos.x, p.contents.dump.bodies.[i+objectEnd].pos.y, 100))
                    AnimationArray.Add(new Hat(p.contents.dump.bodies.[i+objectEnd].pos.x, p.contents.dump.bodies.[i+objectEnd].pos.y))
                    p.contents.dump.bodies.RemoveAt(i+objectEnd)
                targetEnd <- targetEnd - 1
                points.Update(100)
            else
                i <- i + 1
            s <- TargetsArray.Count
        done
        i <- 0; s <- AnimationArray.Count
        while i < s do
            if AnimationArray.[i].PosY > float this.ClientRectangle.Bottom then
                AnimationArray.RemoveAt(i)
            else
                i <- i + 1
            s <- AnimationArray.Count
        done  
        

    do
        this.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint,true)
        //t.Start()
        this.KeyDown.Add(
            fun e -> match e.KeyCode with
                        | Keys.A -> p.contents.dump.bodies.[0].vel <- vec(100., 100.); 
                        | Keys.S -> p.contents.dump.bodies.[0].vel <- vec(-100., -100.);
                        | Keys.D -> p.contents.dump.bodies.RemoveAt(0);
                        | Keys.N -> p.contents.dump.bodies.RemoveAt(0);(!p).add_body(let pos = vec(50., 375.) in
                                                    (pos,0.), phys.def.body 1. [phys.def.circle 20. (vec(0., 0.))] (vec(0., 0.))
                                                   ) |> ignore
                                    (!p).update()
                                    p.contents.dump.bodies.Insert(0, p.contents.dump.bodies.[p.contents.dump.bodies.Count - 1])
                                    p.contents.dump.bodies.RemoveAt(p.contents.dump.bodies.Count - 1)
                        | Keys.O -> angle <- if angle <= 88.f then angle + 2.f else 90.f
                        | Keys.L -> angle <- if angle >= 17.f then angle - 2.f else 15.f
                        | Keys.Z -> this.Zoom(-0.05f)
                        |_ -> ()
           );
                            
        
        this.MouseMove.Add(
            fun x ->
                if initialZoom && gameTime > 5000.f then 
                    zoomingIn <- true
                (*Aiming!*)
                if aimingRect.Contains(x.Location) then
                    let y1 = landscape.[0].Position.y - float landscape.[0].Height/2.
                    let a =float32(Math.Atan2(y1 - float x.Location.Y,float x.Location.X)*180./Math.PI)
                    if a >= 15.f && a <= 90.f then
                        angle <- a
                else
                    isFiring <- false
                    cannonStrength <- 0.f

                 
                if not(started) && initialZoom = false then
                    if viewAllRect.Contains(x.Location) then 
                        if zoom > 0.5f then 
                            this.Zoom(-0.01f); 
                            zoomingOut <- true
                    else
                        if viewSomeRect.Contains(x.Location) then
                            zoomingOut <- false
                            if zoom < 1.f then 
                                this.Zoom(0.01f)
                                zoomingIn <- true
                        else
                            zoomingIn <- false
                            zoomingOut <- false
            
            )
               
    member this.Zoom(h : float32, zpx : float32, zpy : float32) =
        if zoom + h > 0.f then
            zoom <- zoom + h
            zoomPosX <- zpx
            zoomPosY <- zpy
        this.Invalidate()
    
    member this.Stop() = stop <- true

    member this.Run() = 
        if not(stop) then
                let startTime = System.DateTime.Now
                update();
                gameTime <- gameTime + 1000.f/float32 fps
                if gameTime > 10000.f && initialZoom then
                    zoomingIn <- true
                if isFiring then
                    cannonStrength <- if cannonStrength < 1.f then cannonStrength + 0.05f else 0.f                          
                skyObjectsMovement <- (skyObjectsMovement + 1.f) % 1796.f

                if loading then loading <- not(checkLoaded())
                (*Interazione degli ostacoli con i proiettili*)
                for i = landscapeEnd to objectEnd - 1 do
                    let b : body.t = p.contents.dump.bodies.[i]

                    if Math.Abs(b.vel.x) < 5. then b.vel <- vec(0., b.vel.y)
                    if Math.Abs(b.vel.y) < 5. then b.vel <- vec(b.vel.x, 0.)

                    if contactExists 0 i then
                            let o1vel, o1mass = p.contents.dump.bodies.[0].vel, p.contents.dump.bodies.[0].imass
                            let o2vel, o2mass = p.contents.dump.bodies.[i].vel, p.contents.dump.bodies.[i].imass
                            let f1 = 0.5*Math.Pow(Math.Abs(o1vel.x + o1vel.y), 2.)*o1mass
                            let f2 = 0.5*Math.Pow(Math.Abs(o2vel.x + o2vel.y), 2.)*o2mass
                            ObjectsArray.[i-landscapeEnd].Hit(f1 + f2)
                    if explosion && started then
                        let b = bullets.[currentBullet] :?> ExplosiveBullet 
                        p.contents.dump.bodies.[0].vel <- vec(0., 0.)
                        p.contents.dump.bodies.[0].pos <- b.ExplosionPoint
                        if b.ExplosionArea.Contains(pointf p.contents.dump.bodies.[i].pos) then (*Il corpo viene investito dall'esplosione!*)
                            let bpos = p.contents.dump.bodies.[0].pos
                            let opos = p.contents.dump.bodies.[i].pos
                            let vel = vec(float (Math.Sign(opos.x - bpos.x))*300.,- float (Math.Sign(bpos.y - opos.y))*300.)
                            p.contents.dump.bodies.[i].vel <- vel
                            ObjectsArray.[i - landscapeEnd].Hit(ObjectsArray.[i - landscapeEnd].Resistance/3.)
                                
                done
                for i = objectEnd to targetEnd - 1 do
                    let b : body.t = p.contents.dump.bodies.[i]
                    if contactExists 0 i then
                        TargetsArray.[i - objectEnd].Hit(TargetsArray.[i - objectEnd].Resistance)
                (*Poiché questo ciclo scorre una sola volta tutti i target, ne approfittiamo per fare l'animazione ai target*)
                            
                    if explosion && started then
                        let b = bullets.[currentBullet] :?> ExplosiveBullet 
                        p.contents.dump.bodies.[0].vel <- vec(0., 0.)
                        p.contents.dump.bodies.[0].pos <- b.ExplosionPoint
                        if b.ExplosionArea.Contains(pointf p.contents.dump.bodies.[i].pos) then (*Il corpo viene investito dall'esplosione!*)
                            let bpos = p.contents.dump.bodies.[0].pos
                            let opos = p.contents.dump.bodies.[i].pos
                                   
                            TargetsArray.[i - objectEnd].Hit(TargetsArray.[i - objectEnd].Resistance)
                    (TargetsArray.[i - objectEnd] :?> Target).Move(1000./float fps)
                done
                if started then
                    for i = landscapeEnd to objectEnd - 1 do
                        for j = i+1 to objectEnd - 1 do
                            if (not(oldContacts.Contains(contactKey i j)) && p.contents.dump.contacts.ContainsKey(contactKey i j)) && (Math.Abs(p.contents.dump.bodies.[i].vel.x) > 30. || Math.Abs(p.contents.dump.bodies.[i].vel.y) > 30. || Math.Abs(p.contents.dump.bodies.[j].vel.x) > 30. || Math.Abs(p.contents.dump.bodies.[j].vel.y) > 30.)  then (*Il contatto è nuovo*)
                                let o1vel, o1mass = p.contents.dump.bodies.[i].vel, p.contents.dump.bodies.[i].imass
                                let o2vel, o2mass = p.contents.dump.bodies.[j].vel, p.contents.dump.bodies.[j].imass
                                let f1 = 0.5*Math.Pow(Math.Abs(o1vel.x + o1vel.y), 2.)*o1mass
                                let f2 = 0.5*Math.Pow(Math.Abs(o2vel.x + o2vel.y), 2.)*o2mass
                                ObjectsArray.[i - landscapeEnd].Hit((f1 + f2))
                                ObjectsArray.[j - landscapeEnd].Hit((f1 + f2))
                            if p.contents.dump.contacts.ContainsKey(contactKey i j) then
                                if p.contents.dump.contacts.[contactKey i j].jnacc > 0. then ObjectsArray.[i - landscapeEnd].Hits; ObjectsArray.[j - landscapeEnd].Hits (*Dopo maxHits urti qualsiasi ostacolo è distrutto*)
                        done
                        for j = objectEnd to targetEnd - 1 do
                            let mutable enum = p.contents.dump.contacts.GetEnumerator()
                            while enum.MoveNext() do
                                let ct = enum.Current.Value
                                if (ct.a.Equals(p.contents.dump.bodies.[i]) && ct.b.Equals(p.contents.dump.bodies.[j])) || (ct.a.Equals(p.contents.dump.bodies.[j]) && ct.b.Equals(p.contents.dump.bodies.[i])) then
                                    if Math.Abs(p.contents.dump.bodies.[i].vel.x) > 15. || Math.Abs(p.contents.dump.bodies.[i].vel.y) > 15. then
                                        let o1vel, o1mass = ct.a.vel, ct.a.imass
                                        let o2vel, o2mass = ct.b.vel, ct.b.imass
                                        let f1 = 0.5*Math.Pow(Math.Abs(o1vel.x + o1vel.y), 2.)*o1mass
                                        let f2 = 0.5*Math.Pow(Math.Abs(o2vel.x + o2vel.y), 2.)*o2mass
                                        ObjectsArray.[i - landscapeEnd].Hit((f1 + f2))
                                        TargetsArray.[j - objectEnd].Hit((f1 + f2))
                            done
                        done
                        if contactExists 1 i then
                            let o2vel, o2mass = p.contents.dump.bodies.[i].vel, p.contents.dump.bodies.[i].imass
                            let f2 = 0.5*Math.Pow(Math.Abs(o2vel.y + o2vel.x), 2.)*o2mass/10.
                            ObjectsArray.[i - landscapeEnd].Hit(f2)
                              
                    done
                for a in AnimationArray do
                    a.Move(1000./fps)
                done

                deleteAnnihilated()
                        
                let vx = Math.Abs(p.contents.dump.bodies.[0].vel.x)
                let px = p.contents.dump.bodies.[0].pos.x
                let vy = p.contents.dump.bodies.[0].vel.y
                 
                if (vx < 5. && vy = 0. && started && (bullets.[currentBullet].Type <> 1 || (bullets.[currentBullet].Type = 1 && (bullets.[currentBullet] :?> ExplosiveBullet).ExplosionEnded))) || px >= landscape.[0].Width then (*Il proiettile è oramai inusabile. Passiamo al successivo.*)
                    nextBullet() 

                oldContacts.Clear() |> ignore
                for s in p.contents.dump.contacts.Keys do
                    oldContacts.Add(s)
                done

                if zoomingOut && zoom > 0.5f then
                    this.Zoom(-0.01f)
                if zoomingIn && zoom < 1.f then
                    this.Zoom(0.01f)
                if zoom >= 1.f && initialZoom then initialZoom <- false

                this.Invalidate()
                (*Application.DoEvents()
                let endTime = System.DateTime.Now
                let interval = endTime - startTime
                let sleepTime = Math.Max(0, 1000/int fps - int interval.TotalMilliseconds)
                *)
                //System.Threading.Thread.Sleep(sleepTime)

    member this.Zoom(h : float32) =
        (*Il punto rispetto a cui fare lo zoom è sempre il centro*)
        let zpx, zpy = float32 this.ClientRectangle.X + float32 this.ClientRectangle.Width / (2.f * zoom),
                        float32 this.ClientRectangle.Y + float32 this.ClientRectangle.Height / zoom
        this.Zoom(h, zpx, zpy)

    override  this.OnPaint (e) =
        base.OnPaint(e)
        let g = e.Graphics
        let gameLimit = -float32(p.contents.dump.bodies.[1].pos.x + landscape.[0].Width/2.)
        

        let mutable translation = 
            if int p.contents.dump.bodies.[0].pos.x > this.ClientRectangle.Width / 2 && started then 
                -float32 p.contents.dump.bodies.[0].pos.x + float32 this.ClientRectangle.Width / 2.f 
            else
                0.f
        if translation < gameLimit then (*Abbiamo raggiunto il limite*)
            translation <- gameLimit
             

        skytexture.TranslateTransform(translation, 0.f)
        g.FillRectangle(skytexture, new Rectangle(0, 0, this.ClientRectangle.Width, 349))
        skytexture.ResetTransform()

        hillstexture.TranslateTransform(translation,float32 p.contents.dump.bodies.[1].pos.y - 249.f)
        g.FillRectangle(hillstexture, new Rectangle(0,int p.contents.dump.bodies.[1].pos.y - 249, this.ClientRectangle.Width, 249))
        hillstexture.ResetTransform()

        let skyT = if translation > 0.f && p.contents.dump.bodies.[0].vel.x > 0. then translation (*La velocità della pallina, quando è in movimento, è tale per cui non si avverte lo spostamento degli oggetti in cielo*)
                   else -float32 skyObjectsMovement + translation

        skyobjecttexture.TranslateTransform(skyT, 0.f)
        g.FillRectangle(skyobjecttexture, new Rectangle(0, 0, this.ClientRectangle.Width, 482))
        skyobjecttexture.ResetTransform()
      

        if not(started) then
            let startPos = float32 this.ClientRectangle.Width - float32 bullets.Count * 50.f
            let b = new SolidBrush(Color.FromArgb(100, 150, 150, 150))
            g.FillRectangle(b, new RectangleF(startPos - 30.f, 20.f, float32 bullets.Count * 50.f + 30.f, 60.f))
            for i = 0 to bullets.Count - 1  do
                if i < currentBullet then bullets.[i].DrawUsed(g, new PointF(startPos + float32 i * 40.f, 50.f))
                else 
                    if i = currentBullet then bullets.[i].DrawHighlight(g, new PointF(startPos + float32 i * 45.f, 50.f))
                    else bullets.[i].DrawInUse(g, new PointF(startPos + float32 i * 45.f, 50.f))
            done
        
       
        
        g.ScaleTransform(zoom, zoom)
        if zoom <> 1.f then g.TranslateTransform(0.f, -(zoom - 1.f) *float32 this.ClientRectangle.Height/zoom + float32 this.ClientRectangle.Height - float32 p.contents.dump.bodies.[1].pos.y + 25.f - float32(float this.ClientRectangle.Height - p.contents.dump.bodies.[1].pos.y + 25.)/zoom)
        g.TranslateTransform(translation, 0.f, Drawing2D.MatrixOrder.Append)

        
       
        let rotcannon = rotateImage(cannon, angle, new PointF(32.f, 32.f))
        rotcannon.RotateFlip(RotateFlipType.RotateNoneFlipY)
        g.DrawImage(rotcannon, new PointF(0.f, float32 p.contents.dump.bodies.[1].pos.y - 25.f - float32(Math.Cos(float(angle) * Math.PI/180.) * float cannon.Height) - float32(Math.Sin(float(angle) * Math.PI/180.) * float cannon.Width)))
        rotcannon.Dispose()

        let d = p.contents.dump
  
        let line = new Pen(Brushes.Black)
        let green = Brushes.Green
        let f = new Font(FontFamily.GenericSerif, 30.f)
        
        g.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
        //g.Clear(Color.White)
  
        for i = 2 to d.bodies.Count - 1 do
            let b = d.bodies.[i]
            if i >= landscapeEnd && i < objectEnd then
                let o = ObjectsArray.[i - landscapeEnd] :?> Obstacle
                in
                    o.Draw(g, b)
            else
                if i >= objectEnd && i < targetEnd then
                    let t = TargetsArray.[i - objectEnd] :?> Target in t.Draw(g, b)
                else 
                    for s in b.shapes do
                        match s with
                        | shape.Poly p ->
                            g.DrawPolygon(line, 
                                [| for v in p.verts -> pointf v |] )
                        | shape.Circle c ->
                            let r = vec(c.radius,c.radius)
                            g.DrawEllipse(line,
                                RectangleF(pointf (c.center -| r), SizeF(pointf (2. .* r))))
        
       
        if started then 
            bullets.[currentBullet].DrawInUse(g, pointf p.contents.dump.bodies.[0].pos)

        g.DrawImage(cannon_base, new PointF(-30.f, float32 p.contents.dump.bodies.[1].pos.y - 55.f))
        let rotocannon = rotateImage(cannon_over, angle, new PointF(32.f, 32.f))
        rotocannon.RotateFlip(RotateFlipType.RotateNoneFlipY)
        g.DrawImage(rotocannon, new PointF(0.f, float32 p.contents.dump.bodies.[1].pos.y - 25.f - float32(Math.Cos(float(angle) * Math.PI/180.) * float cannon.Height) - float32(Math.Sin(float(angle) * Math.PI/180.) * float cannon.Width)))
        rotocannon.Dispose()
        
        let strength = cannonStrength
        let ldCannon = new Bitmap(cannon_red.Width, cannon_red.Height)
        let ldH = Graphics.FromImage(ldCannon)
        //ldH.DrawImage(cannon_red, new RectangleF(0.f, 0.f, float32 cannon_red.Width * strength, float32 cannon_red.Height))
        ldH.DrawImage(cannon_red, new RectangleF(0.f, 0.f, float32 cannon_red.Width*strength, float32 cannon_red.Height), new RectangleF(0.f, 0.f, float32 cannon_red.Width * strength, float32 cannon_red.Height), GraphicsUnit.Pixel)
        ldH.Dispose()
        let rotldCannon = rotateImage(ldCannon, angle, new PointF(32.f, 32.f))
        rotldCannon.RotateFlip(RotateFlipType.RotateNoneFlipY)
        g.DrawImage(rotldCannon, new PointF(0.f, float32 p.contents.dump.bodies.[1].pos.y - 25.f - float32(Math.Cos(float(angle) * Math.PI/180.) * float cannon.Height) - float32(Math.Sin(float(angle) * Math.PI/180.) * float cannon.Width)))
        rotldCannon.Dispose()
        ldCannon.Dispose()

        
        g.ResetTransform()
        
        dirttexture.TranslateTransform(translation, float32 p.contents.dump.bodies.[1].pos.y - 25.f)
        g.FillRectangle(dirttexture, new Rectangle(0, int p.contents.dump.bodies.[1].pos.y - 25, this.ClientRectangle.Width, this.ClientRectangle.Height - int p.contents.dump.bodies.[1].pos.y + 25))
        dirttexture.ResetTransform()

        grasstexture.TranslateTransform(translation,float32 p.contents.dump.bodies.[1].pos.y - 38.f)
        g.FillRectangle(grasstexture, new Rectangle(0, int p.contents.dump.bodies.[1].pos.y - 38,  this.ClientRectangle.Width, 16))
        grasstexture.ResetTransform()

        g.TranslateTransform(translation, 0.f)
        for a in AnimationArray do
            a.Draw(g)
        done
        g.ResetTransform()
        if loading then
            let ldIm = imageFromName "loading.png" 
            g.DrawImage(ldIm, new Point(this.ClientRectangle.Width / 2 - ldIm.Width /2, this.ClientRectangle.Height / 2 - ldIm.Height/2))
            ldIm.Dispose() 
       
        points.Draw(g) 
        
    override this.OnResize(x) = 
        base.OnResize(x)
        viewAllRect <- new Rectangle(this.ClientRectangle.Width - 50, 0, 50, this.ClientRectangle.Height)
        viewSomeRect <- new Rectangle(0, 0, 50, this.ClientRectangle.Height)


    member this.GameEnded = gameEnded.Publish

    override this.OnMouseDown(x) =
        
        if not(started) then (*Abbiamo un proiettile in attesa!*)
            if checkLoaded() then
                loading <- false
                if aimingRect.Contains(x.Location) then (*Siamo nell'area di tiro*)
                    isFiring <- true
                    zoomingOut <- false
                    zoomingIn <- true
                else
                    isFiring <- false
            else
                loading <- true
        else
            if bullets.[currentBullet].Type <> 1 then nextBullet()
            if bullets.[currentBullet].Type = 1 && p.contents.dump.bodies.[0].pos.x > 200. then
                let b = bullets.[currentBullet] :?> ExplosiveBullet
                if b.Exploded = false then 
                    b.StartExplosion(pointf p.contents.dump.bodies.[0].pos)
                    p.contents.dump.bodies.[0].vel <- vec(0., 0.)
                    explosion <- true
                else if b.ExplosionEnded && contactExists 0 1 then nextBullet()
                        
    override this.OnMouseUp(x) =
        if not(started) && isFiring then
            zoom <- 1.f
            p.contents.dump.bodies.[0].vel <- vec(1000. * float (cannonStrength + 0.2f) * Math.Cos(float angle * Math.PI/180.) ,-1000. * float (cannonStrength + 0.2f) * Math.Sin(float angle * Math.PI/180.) )
            cannonStrength <- 0.f
            isFiring <- false
            started <- true
         
