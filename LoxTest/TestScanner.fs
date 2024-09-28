module LoxFSharp.TestScanner

open LoxFsharp
open NUnit.Framework

[<Test>]
let ``Test Multi-Line Comment`` () =
    let lox = Lox()
    Assert.False lox.hadError

    lox.run
        """
var a = 12;
/* fjdlskafs /*
fjdksla */*/"""

    Assert.False lox.hadError
