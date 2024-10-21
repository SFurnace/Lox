namespace LoxTest

open LoxFsharp
open NUnit.Framework

type TestScanner() =
    inherit TestBase()

    [<Test>]
    member this.``Test Multi-Line Comment``() =
        let program =
            """var a = 12;
    /* fjdlskafs /*
    fjdksla */*/
    """

        let reporter = ErrReporter()
        let scanner = Scanner(program, reporter)
        Assert.False reporter.hadError

        for token in (scanner.scanTokens ()) do
            printf $"%A{token}\n"

        Assert.False reporter.hadError

        this.CheckOutput
            """
{ typ = VAR
  lexme = "var"
  literal = null
  line = 1 }
{ typ = IDENTIFIER
  lexme = "a"
  literal = null
  line = 1 }
{ typ = EQUAL
  lexme = "="
  literal = null
  line = 1 }
{ typ = NUMBER
  lexme = "12"
  literal = 12.0
  line = 1 }
{ typ = SEMICOLON
  lexme = ";"
  literal = null
  line = 1 }
{ typ = EOF
  lexme = ""
  literal = null
  line = 4 }
"""
