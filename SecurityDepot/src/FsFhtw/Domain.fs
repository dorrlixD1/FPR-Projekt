module Domain

open System


type Wertpapier(name: string, isin: string, value: int) =
    member x.Name = name
    member x.Isin = isin
    member x.Value =  value
    override x.ToString() =
        sprintf "(%s, %s, %d)" name isin value

let wertpapiere = 
    [
        Wertpapier("Erste Bank", "AT0000652011", 120);
        Wertpapier("Bank Austira", "AT000B044219", 60);
        Wertpapier("Siemens", "DE0007236101.", 132);
        Wertpapier("Alphabet Inc Class A", "US02079K3059", 82);    
    ]

type depot = List<Wertpapier>
type State = depot

let init () : State =
    []


type Message =
//    | Increment
//    | Decrement
//    | IncrementBy of int
//    | DecrementBy of int
    | SelectWertpapiere of string
 

let filterForSecurityByName = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Name = x )

let update (msg : Message) (model : State) : State =
    match msg with
//    | Increment -> model + 1
//    | Decrement -> model - 1
//    | IncrementBy x -> model + x
//    | DecrementBy x -> model - x
    | SelectWertpapiere x -> filterForSecurityByName x wertpapiere


//    | SelectWertpapiere x -> List.filter (fun isName -> isName = x)
//    | SelectWertpapiere2 x: Wertpapier -> wertpapiere.find (fun y -> y.name = x.name)
//    | SelectWertpapiere x: Wertpapier -> State2.find(fun x -> x = x)