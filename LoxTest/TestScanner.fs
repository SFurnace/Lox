module LoxTest.TestScanner

open LoxFsharp
open NUnit.Framework


[<OneTimeSetUp>]
let ``Setup`` () = System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

[<Test>]
let ``Test Multi-Line Comment`` () =
    let program =
        """var a = 12;
/* fjdlskafs /*
fjdksla */*/
"""

    let reporter = ErrReporter()
    let scanner = Scanner(program, reporter)
    Assert.False reporter.hadError

    for token in (scanner.scanTokens ()) do
        printfn $"%A{token}"

    Assert.False reporter.hadError
