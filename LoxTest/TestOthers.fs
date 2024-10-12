module LoxTest.TestOthers

open System
open Captin.ConsoleIntercept
open NUnit.Framework

[<Test>]
let ``capture stdout`` () =
    using (ConsoleOut.Observe()) (fun observer ->
        Console.WriteLine("'logged' (and original console too)")
        observer.ToString())
    |> ignore
