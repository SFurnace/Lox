namespace LoxTest

open NUnit.Framework
open System.Text.Json
open Newtonsoft.Json

[<TestFixture>]
type T() =
    [<Test>]
    member this.a() =
        let a = JsonSerializer.Create()
        a.Serialize(System.Console.Out, {| a = 12; b = 100 |})
