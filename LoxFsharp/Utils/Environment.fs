namespace LoxFsharp

open System.Collections.Generic
open Microsoft.FSharp.Core

type Environment(enclosing: Option<Environment>) as this =
    let values: IDictionary<string, obj> = Dictionary()

    let ancestor distance =
        let mutable env = this
        let mutable count = 0

        while count < distance do
            if env.enclosing.IsSome then
                env <- env.enclosing.Value
            else
                invalidOp "environment distance is invalid"

            count <- count + 1

        env

    new() = Environment(None)
    member val private enclosing: Option<Environment> = enclosing

    interface LoxEnvironment with
        member this.define(name, value) =
            try
                values.Add(name.lexme, value)
            with :? System.ArgumentException as e ->
                raise (RuntimeError(name, e.Message))

        member this.assign(name, value) =
            if values.ContainsKey name.lexme then
                values[name.lexme] <- value
            elif enclosing.IsSome then
                (enclosing.Value :> LoxEnvironment).assign (name, value)
            else
                raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))

        member this.assignAt(distance, name, value) =
            try
                let env = (ancestor distance) :> LoxEnvironment
                env.assign (name, value)
            with :? System.InvalidOperationException as e ->
                raise (RuntimeError(name, $"Resolve variable '{name.lexme}' failed: {e}."))

        member this.get(name) =
            if values.ContainsKey name.lexme then values[name.lexme]
            elif enclosing.IsSome then (enclosing.Value :> LoxEnvironment).get name
            else raise (RuntimeError(name, $"Undefined variable '{name.lexme}'."))

        member this.getAt(distance: int, name: Token) =
            try
                let env = (ancestor distance) :> LoxEnvironment
                env.get name
            with :? System.InvalidOperationException as e ->
                raise (RuntimeError(name, $"Resolve variable '{name.lexme}' failed: {e}."))
