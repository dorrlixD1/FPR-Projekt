module Repl

open System
open Parser

let header = " Name                         | Typ      | ISIN         | Wert   | Menge "

type Message =
    | MarketMessage of Domain.Message
    | DepotMessage of Domain.Message
    | CalculateMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State

let read (input : string) =
    match input with
    | SelectSecurities (v: string) -> Domain.SelectSecurities v |> MarketMessage
    | ShowAllSecurities -> Domain.ShowAllSecurities |> MarketMessage
    | SellSecurityFromDepot v -> Domain.SellSecurityFromDepot v |> DepotMessage
    | AddSecurityToDepot v -> Domain.AddSecurityToDepot v |> DepotMessage
    | AddSecurityToMarket v -> Domain.AddSecurityToMarket v |> DepotMessage
    | ShowMySecurities -> Domain.ShowMySecurities |> DepotMessage
    | CalculateDepotValue -> Domain.CalculateDepotValue |> CalculateMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")


let evaluate (update : Domain.Message -> State -> State) (state : State) (msg : Message) =
    match msg with
    | MarketMessage msg ->
        let newState: State = update msg state 
        let message: string = sprintf "%A executed. \n\nThe current market is: \n %s \n %A" msg header newState.market 
        (newState, message)
    | DepotMessage msg ->
        let newState: State = update msg state 
        let message: string = sprintf "%A executed. \n\nYour depot contains: \n %s \n %A" msg header newState.depot
        (newState, message)
    | CalculateMessage msg ->
        let newState: State = update msg state 
        let message: string = sprintf "%A executed. \n\nDepot Value: \n %A" msg newState.depotvalue
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : State) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop 

