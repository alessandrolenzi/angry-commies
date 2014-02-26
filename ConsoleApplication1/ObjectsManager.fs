module ObjectsManager

open System
open System.Drawing

open fsphys
open fsphys.geom

open Utilities
open PhysicalObjects


let objectsSelection = imageFromName "objectsSelection.png"
let objectsSelectionHover = imageFromName "objectsSelectionHover.png"
let objectsSelectionSelected = imageFromName "objectsSelectionSelected.png"


type ObjectsManager(x: int, y:int, p: phys.main ref, c:ResizeArray<int>) =
    let mutable posX = x
    let mutable posY = y
    let mutable lastMousePos = new Point()
    let mutable clickPos = new Point()
    let mutable objectsSelectionRect = new Rectangle(x, y, 50, 50)
    let mutable objectsSelectionClicked = false
    let mutable objectsListRect = new Rectangle(x+55, y, 375,250)

    let mutable objectsRects = new ResizeArray<Rectangle>()
    let mutable objectsImages = new ResizeArray<Bitmap>()
    let mutable obstacles = new ResizeArray<Obstacle>()

    let mutable mousedown = false
    let mutable dragging = false
    let mutable draggingItem = 0
    let mutable cursor = new Bitmap(1, 1)

    let mutable zoom = 1.f
    let mutable zoomTranslation = 0.f
    
    let mutable translation = 0.f

    let objectAdded = new Event<Point>()

    let initObjectsRects() =
        objectsRects.Clear()
        for j = 0 to 1 do
            for i = 0 to 3 do
                objectsRects.Add(new Rectangle(new Point(objectsListRect.X + 15 + i * 90, objectsListRect.Y + 55 + 90*j), new Size(80, 80)))
    do
        initObjectsRects()
        objectsImages.Add(resizeImage(imageFromName "woodLittle.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "woodLong.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "woodVeryLong.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "woodCircle.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "glassLittle.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "glassLong.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "glassVeryLong.png", 80, 80))
        objectsImages.Add(resizeImage(imageFromName "glassCircle.png", 80, 80))
    
    member this.ObjectAdded = objectAdded.Publish

    member this.RemoveAt(i:int) =
        (*Determiniamo qual è il numero locale dell'oggetto*)
        let mutable k = -1
        for j = 1 to i do
            if c.[j] = 0 then
                k <- k + 1
        done
        if k < obstacles.Count && k >= 0 then
            obstacles.RemoveAt(k)
            c.RemoveAt(i)
    
    member this.Obstacles with get() = obstacles
      
    member this.Draw(g:Graphics) = 
       
        if objectsSelectionClicked then
            g.DrawImage(objectsSelectionSelected, objectsSelectionRect)
            g.FillRectangle(controlBrush, objectsListRect)
            g.DrawString("Aggiunta ostacoli", bigFont, Brushes.Black, new PointF(float32 objectsListRect.X + 10.f,float32 objectsListRect.Y + 10.f))
            for i = 0 to Math.Min(objectsImages.Count, objectsRects.Count) - 1 do
                if objectsRects.[i].Contains(lastMousePos) then g.FillRectangle(hoverBrush, objectsRects.[i])
                if i + 1 = draggingItem then g.DrawRectangle(Pens.Gold, objectsRects.[i])
                g.DrawImage(objectsImages.[i], new PointF(float32 objectsRects.[i].X + float32 objectsRects.[i].Width / 2.f - float32 objectsImages.[i].Width/2.f, float32 objectsRects.[i].Y + float32 objectsRects.[i].Height / 2.f - float32 objectsImages.[i].Height/2.f))
            done
        else
            if objectsSelectionRect.Contains(lastMousePos) then 
                g.DrawImage(objectsSelectionHover, objectsSelectionRect)
                g.FillRectangle(hoverBrush, new RectangleF(new PointF(float32 lastMousePos.X, float32 lastMousePos.Y - 5.f), new SizeF(150.f, 30.f)))
                g.DrawString("Aggiunta ostacoli", controlFont, Brushes.White, new PointF(float32 lastMousePos.X + 10.f, float32 lastMousePos.Y))
            else 
                g.DrawImage(objectsSelection, objectsSelectionRect)



    member this.SetZoom(z:float32, zoomT : float32) = zoom <- z; zoomTranslation <- zoomT
    member this.SetTranslation(t:float32) = translation <- t
    
    member this.Click(p: Point) = 
        clickPos <- p
        if objectsSelectionRect.Contains(p) then
            objectsSelectionClicked <- not(objectsSelectionClicked)
        else if objectsListRect.Contains(p) = false then
            objectsSelectionClicked <- false

    member this.MouseUp(loc : Point) =
        if dragging then
            dragging <- false
            let mutable added = false
            if objectsSelectionClicked = false then (*L'oggetto è stato a tutti gli effetti aggiunto al gioco*)
                cursor.Dispose()
                added <- true
                let obs : Obstacle = 
                    match draggingItem with
                        | 1 -> new LittleWoodObstacle(vec(float loc.X,float loc.Y)) :> Obstacle
                        | 2 -> new LongWoodObstacle(vec(float loc.X,float loc.Y)) :> Obstacle
                        | 3 -> new VeryLongWoodObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | 4 -> new CircleWoodObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | 5 -> new LittleGlassObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | 6 -> new LongGlassObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | 7 -> new VeryLongGlassObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | 8 -> new CircleGlassObstacle(vec(float loc.X,float loc.Y)):> Obstacle
                        | _ -> new LittleGlassObstacle(vec(float loc.X,float loc.Y)) :> Obstacle
                obstacles.Add(obs)
                (!p).add_body((vec(float loc.X/float zoom - float translation, float loc.Y/float zoom - float zoomTranslation) ,0.), phys.def.body ((obs :> IGameElement).Density) [(obs :> IGameElement).Shape] (vec(0., 0.)))|> ignore
                c.Add(0)
                (!p).update()
                
            cursor <- new Bitmap(1, 1)
            draggingItem <- 0
            if added then objectAdded.Trigger(loc)

    member this.MouseDown(p: Point) =
        mousedown <- true
        
        if objectsSelectionClicked then
            for i = 0 to objectsRects.Count - 1 do
                if objectsRects.[i].Contains(p) && dragging = false then (*Inizio drag & drop*)
                    dragging <- true
                    draggingItem <- i + 1
                    cursor <- objectsImages.[i]
            done 

    member this.MouseMove(p: Point) =
        lastMousePos <- p
        if dragging && not(objectsListRect.Contains(p)) then (*Come usciamo dall'area l'oggetto diventa intero*)
            objectsSelectionClicked <- false
            cursor <- match draggingItem with
                        | 1 -> imageFromName "woodLittle.png"
                        | 2 -> imageFromName "woodLong.png"
                        | 3 -> imageFromName "woodVeryLong.png"
                        | 4 -> imageFromName "woodCircle.png"
                        | 5 -> imageFromName "glassLittle.png"
                        | 6 -> imageFromName "glassLong.png"
                        | 7 -> imageFromName "glassVeryLong.png"
                        | 8 -> imageFromName "glassCircle.png"
                        | _ -> new Bitmap(1, 1)
    
    member this.Position(x:int, y:int) = 
        posX <- x
        posY <- y
        objectsSelectionRect <- new Rectangle(new Point(x, y), new Size(50, 50))
        objectsListRect <- new Rectangle(new Point(x+55, y), new Size(375, 250))
        initObjectsRects()
       
    interface IDisposable with
        member this.Dispose() =
            for i in objectsImages do i.Dispose()

    end

    member this.DrawBodies(g:Graphics) = 
        let mutable j = 0
        for i = 1 to p.contents.dump.bodies.Count - 1  do
            if c.[i] = 0 then
                obstacles.[j].Draw(g, p.contents.dump.bodies.[i])
                j <- j + 1
        done

        if dragging then 
            g.DrawImage(cursor, new PointF(float32 lastMousePos.X/zoom - float32 cursor.Width / 2.f - translation, float32 lastMousePos.Y/zoom - float32 cursor.Height/2.f - zoomTranslation))
