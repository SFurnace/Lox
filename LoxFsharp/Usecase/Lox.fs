namespace LoxFsharp

open System.IO

type Lox() =
    let reporter = ErrReporter()
    let interpreter = Interpreter(reporter)

    member this.hadError = reporter.hadError

    member this.run text =
        let scanner = Scanner(text, reporter)
        let tokens = scanner.scanTokens ()
        let parser = Parser(tokens, reporter)
        let expr = parser.parse ()

        if not this.hadError && expr.IsSome then
            let v = interpreter.interpret expr.Value
            printfn $"%s{Utils.stringify v}"

    member this.runFile path =
        File.ReadAllText path |> this.run

        if reporter.hadError then
            exit 65

        if reporter.hadRuntimeError then
            exit 70

    [<TailCall>]
    member this.runPrompt() =
        printf "> "

        match stdin.ReadLine() with
        | null -> ()
        | line ->
            this.run line
            reporter.hadError <- false
            this.runPrompt ()
