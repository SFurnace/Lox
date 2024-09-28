module LoxFsharp.Program

[<EntryPoint>]
let main args =
    let lox = Lox()

    match args with
    | [||] -> lox.runPrompt ()
    | [| file |] -> lox.runFile file
    | _ ->
        eprintfn "Usage: lox [script]"
        exit 64

    0
