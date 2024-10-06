namespace LoxFsharp

open System.Collections.Generic

type Environment(enclosing: Option<LoxEnvironment>) =
    let enclosing = enclosing
    let values = Dictionary<string, obj>()

    new() = Environment(None)

    interface LoxEnvironment with
        member this.define(name, value) =
            try
                values.Add(name.lexme, value)
            with :? System.ArgumentException as e ->
                raise (RuntimeError(name, e.Message))

        member this.assign(name, value) =
            if values.ContainsKey name.lexme then values[name.lexme] <- value
            elif enclosing.IsSome then enclosing.Value.assign (name, value)
            else raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))

        member this.get(name: Token) =
            if values.ContainsKey name.lexme then values[name.lexme]
            elif enclosing.IsSome then enclosing.Value.get name
            else raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))
