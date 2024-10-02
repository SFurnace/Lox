module LoxTest.TestScanner

open NUnit.Framework

[<Test>]
let ``Test Multi-Line Comment`` () =
    let program =
        """var a = 12;
/* fjdlskafs /*
fjdksla */*/
"""

    let reporter = LoxFsharp.ErrReporter()
    let scanner = LoxFsharp.Scanner(program, reporter)
    Assert.False reporter.hadError

    for token in (scanner.scanTokens ()) do
        printfn $"%A{token}"

    Assert.False reporter.hadError
