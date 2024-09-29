namespace LoxFsharp

open Microsoft.FSharp.Core

type ErrReporter() =
    member val hadError = false with get, set

    member this.report line where message =
        printfn $"[line {line}] Error{where}: {message}"
        this.hadError <- true

    member this.error(line: int, message: string) = this.report line "" message

    member this.error(token: Token, message: string) =
        if token.typ = TokenType.EOF then
            this.report token.line " at end" message
        else
            this.report token.line $" at '{token.lexme}'" message
