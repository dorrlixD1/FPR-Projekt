module Domain

open System

//Variables & Lists
type Wertpapier(name: string, isin: string, value: int) =
    member x.Name = name
    member x.Isin = isin
    member x.Value =  value
    override x.ToString() =
        sprintf "(%s, %s, %d)" name isin value

let wertpapiere = 
    [
        Wertpapier("ErsteBank", "AT0000652011", 120);
        Wertpapier("ErsteBank", "AT0000652012", 120);
        Wertpapier("BankAustria", "AT000B044219", 60);
        Wertpapier("Siemens", "DE0007236101.", 132);
        Wertpapier("AlphabetIncClassA", "US02079K3059", 82);    
    ]
    

//State -> Wertpapier Depot    
type State = List<Wertpapier>
let init () : State =
    []

// Message Pattern Matching
type Message =
    | AddSecurity of string
    | ShowMySecurities

type Message2 =
    | SelectWertpapiere of string
    | ShowAllSecurities 

//Functions
let filterForSecurityByName = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Name = x )
let filterForSecurityByIsin = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Isin = x )

// let addSecurity x (y: Wertpapier list) (z: Wertpapier list) = 
    


//Delegates
let update (msg : Message) (model : State) : State =
    match msg with
    | AddSecurity x  -> model @ filterForSecurityByIsin x wertpapiere
    | ShowMySecurities -> model


// Versuch --> Kein Update vom State, sondern nur Ausgabe einer Liste --> Info
let information (msg : Message2) =
    match msg with
    | SelectWertpapiere x -> filterForSecurityByName x wertpapiere
    | ShowAllSecurities -> wertpapiere
