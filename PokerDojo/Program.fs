// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type CardColor =
    | Heart = 0     // Herz
    | Tile = 1      // Karo
    | Clover = 2    // Kreuz
    | Pike = 3      // Pik


type Card(name: string) =
    let textName = name

    let parseChar (c : char) : int = System.Int32.Parse(c.ToString())

    let GetValue x =
        match x with
        | 'T' -> 10 // Ten
        | 'J' -> 10 // Jack
        | 'Q' -> 10 // Queen
        | 'K' -> 10 // King
        | 'A' -> 11 // Ace
        | x when parseChar x < 10 -> parseChar x
        | _ -> raise (System.ArgumentException "unknown card value")

    let GetColor c =
        match c with
        | 'H' -> CardColor.Heart
        | 'T' -> CardColor.Tile
        | 'C' -> CardColor.Clover
        | 'P' -> CardColor.Pike
        | _ -> raise (System.ArgumentException "color unknown")

    member this.Name = textName
    member this.CardValue with get() = GetValue(this.Name.Chars 0)
    member this.Color with get() = GetColor(this.Name.Chars 1)
    
let GetHighestCard cl : Card = (cl |> List.maxBy (fun t -> t.CardValue))


let hand = [ Card("JH") ; Card("AH") ]
hand |> Seq.map (fun f -> f.Name) |> Seq.iter (printfn "%A")
hand |> GetHighestCard |> (fun t -> t.Name) |> printfn "%A"


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
