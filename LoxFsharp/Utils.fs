module LoxFsharp.Utils

type ErrReporter() =
    member val hadError = false with get, set

    member this.report line where message =
        printfn $"[line {line}] Error{where}: {message}"
        this.hadError <- true

    member this.error line message = this.report line "" message
