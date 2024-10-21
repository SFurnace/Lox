module LoxFsharp.Main

[<EntryPoint>]
let main args =
    let lox = Lox()

    match args with
    | [||] -> lox.runPrompt ()
    | [| file |] -> lox.runFile file
    | _ ->
        eprintf "Usage: lox [script]\n"
        exit 64

    0
