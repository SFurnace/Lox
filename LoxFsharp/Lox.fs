module Lox.Main

open System.IO

let run text = printfn $"{text}"

let runFile path = path |> File.ReadAllText |> run

[<TailCall>]
let rec runPrompt () =
    printf "> "

    match stdin.ReadLine() with
    | null -> ()
    | line ->
        run line
        runPrompt ()

[<EntryPoint>]
let main args =
    match args with
    | [||] -> runPrompt ()
    | [| file |] -> runFile file
    | _ ->
        eprintfn "Usage: lox [script]"
        exit 64

    0
