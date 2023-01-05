module Parser

open System
open Domain

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

//|Increment|Decrement|IncrementBy|DecrementBy|
let (|Help|ParseFailed|SelectSecurities|AddSecurityToDepot|ShowAllSecurities|ShowMySecurities|SellSecurityFromDepot|) (input : string) =
    let tryParseInt (arg2 : string) valueConstructor =
        let (worked, arg2') = Int32.TryParse arg2
        if worked then valueConstructor arg2' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with    
    | [ verb; arg ] when safeEquals verb (nameof Domain.SelectSecurities) -> SelectSecurities arg
    | [ verb ] when safeEquals verb (nameof Domain.ShowAllSecurities) -> ShowAllSecurities
    | [ verb ] when safeEquals verb (nameof Domain.ShowMySecurities) -> ShowMySecurities
    | [ verb; arg: string ; arg2: string] when safeEquals verb (nameof Domain.AddSecurityToDepot) ->
        tryParseInt arg2 (fun value -> AddSecurityToDepot (arg, value))
    | [ verb; arg ; arg2] when safeEquals verb (nameof Domain.SellSecurityFromDepot) -> 
        tryParseInt arg2 (fun value -> SellSecurityFromDepot (arg, value))
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed


let (|ParseFailed|AddSecurityToMarket|CalculateDepotValue|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let tryParseWertpapierTyp (x: string) = 
        match x with
        | "Aktie" -> Aktie
        | "Anleihen" -> Anleihen
        | "Zertifikate" -> Zertifikate
        | "Fond" -> Fond
        | "ETF" -> ETF
        | "" -> None
        | _ -> None

    let parts = input.Split(' ') |> List.ofArray
    match parts with    
    | [ verb ] when safeEquals verb (nameof Domain.CalculateDepotValue) -> CalculateDepotValue
    | [ verb; arg ; arg2 ; arg3, arg4, arg5] when safeEquals verb (nameof Domain.AddSecurityToMarket) -> 
        AddSecurityToMarket (arg,
        //arg2,arg3,arg4,arg5)
            arg, 
            tryParseWertpapierTyp arg2 (fun value -> value), 
            arg3, 
            tryParseInt arg4 (fun value -> value), 
            tryParseInt arg5 (fun value -> value))
    | _ -> ParseFailed


    
 //   let converted = Domain.WertpapierTyp input
 //   let tryParseWertpapierTyp (arg : string) valueConstructor =  
//	let (worked, arg') =
 //            match arg' with
 //            | :? msg as WertpapierTyp 
   //          | ParseFailed    
      