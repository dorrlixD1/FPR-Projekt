module Domain

open System

// Domainspecific Declaration
type WertpapierTyp =
    | Aktie
    | Anleihen
    | Zertifikate
    | Fond
    | ETF
    | None
    override x.ToString() = ""

type Wertpapier(name: string, typ: WertpapierTyp,  isin: string, value: int, amount: int ) =
    member x.Name = name
    member x.Typ = typ
    member x.Isin = isin
    member x.Value = value
    member x.Amount = amount
    override x.ToString() =
        sprintf "%-30s %-10A %-14s %-8d %-7d" name typ isin value amount 

// Message Pattern Matching

    
type Message =     
    | AddSecurityToDepot of isin : string * amount : int
    | ShowMySecurities
    | SellSecurityFromDepot of 
        isin : string * 
        amount : int
    | SelectSecurities of string
    | ShowAllSecurities
    | AddSecurityToMarket of 
        name : string * 
        typ : string * 
        isin : string * 
        value : int * 
        amount : int
    | CalculateDepotValue


 
//State -> Wertpapier Depot & markets
type State(depot: List<Wertpapier>, market: List<Wertpapier>, depotvalue: int) = 
    member x.depot = depot
    member x.market = market
    member x.depotvalue = depotvalue


//Initialization
let init () : State =
    new State(
        [
            
        ],    
        [
            Wertpapier("ErsteBank",  Aktie , "AT0000652011", 120, 67);
            Wertpapier("ErsteBank", Aktie ,"AT0000652012", 120, 7);
            Wertpapier("BankAustria", Aktie ,"AT000B044219", 60 , 4);
            Wertpapier("Siemens", Aktie ,"DE0007236101.", 132 , 44);
            Wertpapier("AlphabetIncClassA", Aktie , "US02079K3059", 82, 2); 
            Wertpapier("ERSTEGREENINVEST", Fond , "AT0000A2DY59", 250, 2); 
        ], 
        0)



//Functions
let filterForSecurityByName = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Name = x )
let filterForSecurityByIsin = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Isin = x )


// let rec addToDepot x = x + x
// let calculateDepotValue  = fun (x: State) -> new State (x.depot, x.market,
//    addToDepot x.depot

//let sum list:Wertpapier = List.sumBy (fun elem -> elem*elem)

let rec calculateDepotValue x = 
    match x with
    | (head:Wertpapier) :: tail -> (head.Amount * head.Value + (calculateDepotValue tail))
    | _ -> 0

//let removeSecurityFromMarket = fun 

// let sellSecurity =  
// Wildcard Search bei SelectSecurity offen 


// let addSecurity x (y: Wertpapier list) (z: Wertpapier list) = 

// type addSecuritytoDepot = 


//Delegates
let update (msg : Message) (model : State) : State =
    match msg with
//    | AddSecurityToDepot x  -> new State(model.depot @ filterForSecurityByIsin x model.market,model.market)
    | AddSecurityToDepot (x: string, y: int) -> model
    | ShowMySecurities -> model
    | SelectSecurities x -> new State (model.depot, filterForSecurityByName x model.market, model.depotvalue)
    | ShowAllSecurities -> model
    | SellSecurityFromDepot (x: string, y) -> model
    | AddSecurityToMarket (name, typ, isin, value, amount) -> model
    | CalculateDepotValue -> new State(model.depot, model.market, calculateDepotValue model.depot)
    
    // addSecurityToDepot X X (ISIN, Amount) |> DecrementBy x for model.market  |> filterForSecurityByISIN List<Wertpapier> |> IncrementBy x model.depot
    // addSecurityToMarket X:Wertpapier 
    // CalculateDepotValue |> sumAmount (List<Wertpapier>)
    // SellSecurityFromDepot X X (ISIN, Amount) |> DecrementBy x for model.market  |> filterForSecurityByISIN List<Wertpapier> |> IncrementBy x model.depot
    // SelectSecurities x // wildcard search

    