module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | DomainMessage2 of Domain.Message2
    | HelpRequested
    | NotParsable of string

type State = Domain.State

let read (input : string) =
    match input with
    | SelectWertpapiere (v: string) -> Domain.SelectWertpapiere v |> DomainMessage2
    | ShowAllSecurities -> Domain.ShowAllSecurities |> DomainMessage2
    | AddSecurity (v: string) -> Domain.AddSecurity v |> DomainMessage
    | ShowMySecurities -> Domain.ShowMySecurities |> DomainMessage
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
    | DomainMessage msg ->
        let newState: State = update msg state
        let message: string = sprintf "The operation used was %A. Your depot contains %A" msg newState
        (newState, message)
    | DomainMessage2 msg ->
        let newState2: State = Domain.information msg
        let message = sprintf "The operation2 used was %A. These are the available securities %A" msg newState2
        (newState2, message)
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
