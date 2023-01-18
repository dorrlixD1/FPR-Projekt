module Domain

open System

// Domainspecific Declarations
type WertpapierTyp =
    | Aktie
    | Anleihen
    | Zertifikate
    | Fond
    | ETF
    | None
    override x.ToString() = ""

// Security class
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
            Wertpapier("ErsteBank",  Aktie , "AT0000652011", 120, 67)
        ],    
        [
            Wertpapier("ErsteBank",  Aktie , "AT0000652011", 120, 67);
            Wertpapier("BankAustria", Aktie ,"AT000B044219", 60 , 4);
            Wertpapier("Siemens", Aktie ,"DE0007236101.", 132 , 44);
            Wertpapier("AlphabetIncClassA", Aktie , "US02079K3059", 82, 2); 
            Wertpapier("ERSTEGREENINVEST", Fond , "AT0000A2DY59", 250, 2); 
        ], 
        0)

// Parser
let tryParseWertpapierTyp (x: string) = 
        match x with
        | "Aktie" -> Aktie
        | "Anleihen" -> Anleihen
        | "Zertifikate" -> Zertifikate
        | "Fond" -> Fond
        | "ETF" -> ETF
        | "" -> None
        | _ -> None

// Functions
let filterForSecurityByName = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Name = x )
let filterForSecurityByIsin = fun  x  (z: Wertpapier list) -> z |> List.filter ( fun y -> y.Isin = x )

let rec calculateDepotValue x = 
    match x with
    | (head:Wertpapier) :: tail -> (head.Amount * head.Value + (calculateDepotValue tail))
    | _ -> 0

let addSecurityToMarket  =
    fun name typ isin value amount  market ->
        new Wertpapier(name,tryParseWertpapierTyp typ,isin,value,amount) :: market

let findWertpapier = 
    fun isin (market: Wertpapier list) -> 
        market |> List.find (fun y -> y.Isin = isin)

let generateNewWertpapier = 
    fun (wertpapier : Wertpapier) ->
        new Wertpapier(wertpapier.Name, wertpapier.Typ, wertpapier.Isin, wertpapier.Value, wertpapier.Amount)

let decrementAmountofSecurity (list : Wertpapier list) (isin : String) (amount : int) =
    let rec decrementAmountofSecurity list list' =
        match list with
        | [] -> list'
        | (head:Wertpapier) :: tail -> if head.Isin.Equals(isin) then                                  
                                                                tail @ (list' @ [new Wertpapier(head.Name, head.Typ, head.Isin, head.Value, (head.Amount - amount))]) // decrement amount from element
                                                        else
                                                            decrementAmountofSecurity tail (list' @ [head]) // keep seachring
    decrementAmountofSecurity list []

let incrementAmountofSecurity (list : Wertpapier list) (isin : String) (amount : int) =
    let rec incrementAmountofSecurity list list' =
        match list with
        | [] -> list'
        | (head:Wertpapier) :: tail -> if head.Isin.Equals(isin) then                                  
                                                                tail @ (list' @ [new Wertpapier(head.Name, head.Typ, head.Isin, head.Value, (head.Amount + amount))]) // increment amount from element
                                                        else
                                                            incrementAmountofSecurity tail (list' @ [head]) // keep seachring
    incrementAmountofSecurity list []

let addSecurityToDepot (state : State) (isin : string) (amount : int)=
    if (state.market |> List.exists(fun(x) -> x.Isin.Equals(isin) && x.Amount >= amount)) then
        let newSecurity : Wertpapier  = generateNewWertpapier(findWertpapier isin state.market)
        let isSecurityExistingInDepot = state.depot |> List.exists(fun(x) -> x.Isin.Equals(isin))
        new State( (if (isSecurityExistingInDepot) then incrementAmountofSecurity state.depot isin amount else newSecurity :: state.depot) , decrementAmountofSecurity state.market isin amount, state.depotvalue) 
    else
        new State(state.depot, state.market , state.depotvalue) 

// done
let sellSecurityFromDepot (depot : Wertpapier list) (isin : String) (amount : int) =
    let rec sellSecurityFromDepot depot depot' =
        match depot with
        | [] -> depot'
        | (head:Wertpapier) :: tail -> if head.Isin.Equals(isin) then
                                                            if head.Amount > amount then
                                                                tail @ (depot' @ [new Wertpapier(head.Name, head.Typ, head.Isin, head.Value, (head.Amount - amount))]) // decrement amount from element
                                                            else
                                                                depot' @ tail // remove the element
                                                        else
                                                            sellSecurityFromDepot tail (depot' @ [head]) // keep seachring
    sellSecurityFromDepot depot []
    
let update (msg : Message) (model : State) : State =
    match msg with
    | ShowMySecurities -> model // Done
    | SelectSecurities isin -> new State (model.depot, filterForSecurityByName isin model.market, model.depotvalue) // Done
    | ShowAllSecurities -> model // Done
    | SellSecurityFromDepot (isin, amount) -> new State(sellSecurityFromDepot model.depot isin amount ,  model.market , model.depotvalue) // Done
    | AddSecurityToMarket (name, typ, isin, value, amount) -> new State(model.depot, addSecurityToMarket name typ isin value amount model.market, model.depotvalue) //Working prob. needs Testing
    | AddSecurityToDepot (isin, amount) -> addSecurityToDepot model isin amount
    | CalculateDepotValue -> new State(model.depot, model.market, calculateDepotValue model.depot) //Done    

    