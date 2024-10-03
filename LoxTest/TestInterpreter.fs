namespace LoxTest

open LoxFsharp
open NUnit.Framework

[<TestFixture>]
type TestInterpreter() =

    [<OneTimeSetUp>]
    member this.``Setup``() = System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

    [<Test>]
    member this.``Test Run File``() =
        let lox = Lox()
        lox.runFile "programs\1.lox"
