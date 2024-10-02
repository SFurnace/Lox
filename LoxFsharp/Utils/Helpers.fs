module LoxFsharp.Utils

open System
open Microsoft.FSharp.Core

let isTruthy (v: obj) = not (v = null || v = false)

let isEqual x y = obj.Equals(x, y)

let stringify (o: obj) =
    let s = $"{o}"
    if (o :? float) && s.EndsWith ".0" then s.Substring(0, s.Length - 2) else s

let checkNumberOperands operatorToken (operands: obj[]) =
    let invalidOps = operands |> Array.filter (fun x -> not (x :? float))

    if invalidOps.Length > 0 then
        raise (RuntimeError(operatorToken, "Operand must be a number."))
