module LoxFsharp.Utils

open Microsoft.FSharp.Core

let IsTruthy (v: obj) = not (v = null || v = false)

let IsEqual x y = obj.Equals(x, y)

let Stringify (o: obj) =
    match o with
    | null -> "nil"
    | :? float ->
        let s = $"{o}"
        if (o :? float) && s.EndsWith ".0" then s.Substring(0, s.Length - 2) else s
    | :? bool -> $"{o}".ToLower()
    | _ -> $"{o}"

let CheckNumberOperands operatorToken (operands: obj[]) =
    let invalidOps = operands |> Array.filter (fun x -> not (x :? float))

    if invalidOps.Length > 0 then
        raise (RuntimeError(operatorToken, "Operand must be a number."))
