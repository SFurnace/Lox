namespace LoxFsharp

open System.Collections.Generic
open Microsoft.FSharp.Core

type Environment(enclosing: Option<Environment>) =
    let enclosing = enclosing
    let values = Dictionary<string, obj>()

    member this.define name value = values[name] <- value

    member this.assign name value =
        if values.ContainsKey name.lexme then values[name.lexme] <- value
        elif enclosing.IsSome then enclosing.Value.assign name value
        else raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))

    member this.get(name: Token) =
        if values.ContainsKey name.lexme then values[name.lexme]
        elif enclosing.IsSome then enclosing.Value.get name
        else raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))

    new() = Environment(None)
