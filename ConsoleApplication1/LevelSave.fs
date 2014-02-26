module LevelSave

open System
open System.IO

open fsphys
open fsphys.geom

open PhysicalObjects


type LevelSave(title:string, maxGameWidth : float, p: phys.main ref, obstacles : ResizeArray<Obstacle>, bullets : ResizeArray<Bullet>, targets : ResizeArray<Target>, corr: ResizeArray<int>) =
    let handle = new StreamWriter(@"C:\Users\User\AngryLevels\level.txt")
    
    do 
        handle.WriteLine(title + sprintf ":%f" maxGameWidth) (*Scriviamo titolo e lunghezza di gioco in modo da inserire il landscape*)
        p.contents.dump.bodies.RemoveAt(0)
        corr.RemoveAt(0)
         (*Eliminiamo il landscape che non ce ne facciamo niente*)
        handle.WriteLine()
        let mutable j = 0
        let mutable k = 0
        for i = 0 to p.contents.dump.bodies.Count - 1 do
            let b = p.contents.dump.bodies.[i]
            let mutable kind = -1
            if corr.[i] = 0 then
                kind <- obstacles.[j].Kind
                j <- j + 1
            else
                kind <- targets.[k].Kind
                k <- k + 1
            handle.WriteLine(sprintf "%.2f:%.2f:%.5f:%d" b.pos.x b.pos.y b.ang kind) (*Le altre informazioni le traiamo dal tipo di oggetto*)
        done
        handle.WriteLine()
        let mutable s : string = ""
        for b in bullets do
            s <- s + sprintf "%d:" b.Type
        done        
        handle.WriteLine(s)
        handle.WriteLine()
        handle.Close()
        handle.Dispose()

    member this.Done with get() = true
    
