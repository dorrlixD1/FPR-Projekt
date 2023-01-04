module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

//|Increment|Decrement|IncrementBy|DecrementBy|
let (|Help|ParseFailed|SelectWertpapiere|AddSecurity|ShowAllSecurities|ShowMySecurities|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; arg ] when safeEquals verb (nameof Domain.SelectWertpapiere) -> SelectWertpapiere arg
    | [ verb ] when safeEquals verb (nameof Domain.ShowAllSecurities) -> ShowAllSecurities
    | [ verb ] when safeEquals verb (nameof Domain.ShowMySecurities) -> ShowMySecurities
    | [ verb; arg ] when safeEquals verb (nameof Domain.AddSecurity) -> AddSecurity arg
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed
