[<EntryPoint>]
let main argv =
    printfn "Welcome to the Ultimate Wertpapier Depot Manager"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printf "> "

    let initialState = Domain.init ()
    Repl.loop initialState
    0 // return an integer exit code
