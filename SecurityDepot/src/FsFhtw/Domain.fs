module Domain

open System

type WertpapierTyp =
    | Aktie
    | Anleihen 
    | Zertifikate
    | Fond
    | ETF

//Variables & Lists
type Wertpapier(name: string, typ: WertpapierTyp,  isin: string, value: int) =
    member x.Name = name
    member x.Typ = typ
    member x.Isin = isin
    member x.Value =  value
    override x.ToString() =
        sprintf "(%s, %s, %d)" name isin value
 
//State -> Wertpapier Depot    
type State(depot: List<Wertpapier>, market: List<Wertpapier>) = 
    member x.depot = depot
    member x.market = market


//Initialization
let init () : State =
    new State(
        [
            
        ],    
        [
            Wertpapier("ErsteBank", Aktie , "AT0000652011", 120);
            Wertpapier("ErsteBank", Aktie ,"AT0000652012", 120);
            Wertpapier("BankAustria", Aktie ,"AT000B044219", 60);
            Wertpapier("Siemens", Aktie ,"DE0007236101.", 132);
            Wertpapier("AlphabetIncClassA", Aktie , "US02079K3059", 82); 
            Wertpapier("ERSTEGREENINVEST", Fond , "AT0000A2DY59", 250); 
        ])


// Message Pattern Matching
type Message =
    | AddSecurity of string
    | ShowMySecurities
    | SellSecurity of string
    | SelectWertpapiere of string
    | ShowAllSecurities


//Functions
let filterForSecurityByName = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Name = x )
let filterForSecurityByIsin = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Isin = x )
//let removeSecurityFromMarket = fun 

// let sellSecurity = 
// Wildcard Search bei SelectSecurity offen 


// let addSecurity x (y: Wertpapier list) (z: Wertpapier list) = 
    


//Delegates
let update (msg : Message) (model : State) : State =
    match msg with
    | AddSecurity x  -> new State(model.depot @ filterForSecurityByIsin x model.market,model.market)
    | ShowMySecurities -> model
    | SellSecurity x -> model
    | SelectWertpapiere x -> new State (model.depot, filterForSecurityByName x model.market)
    | ShowAllSecurities -> model
    