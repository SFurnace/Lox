module rec LoxFsharp.Lox

open System.IO
open LoxFsharp.Utils
open LoxFsharp.Scanner

type Lox() =
    let reporter = ErrReporter()

    member this.run text =
        let scanner = Scanner(text, reporter)
        let tokens = scanner.scanTokens ()

        for token in tokens do
            printfn $"%A{token}"

    member this.runFile path =
        File.ReadAllText path |> this.run

        if reporter.hadError then
            exit 65

    [<TailCall>]
    member this.runPrompt() =
        printf "> "

        match stdin.ReadLine() with
        | null -> ()
        | line ->
            this.run line
            reporter.hadError <- false
            this.runPrompt ()

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
