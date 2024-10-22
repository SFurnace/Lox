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
  lexeme = "var"
  literal = null
  line = 1 }
{ typ = IDENTIFIER
  lexeme = "a"
  literal = null
  line = 1 }
{ typ = EQUAL
  lexeme = "="
  literal = null
  line = 1 }
{ typ = NUMBER
  lexeme = "12"
  literal = 12.0
  line = 1 }
{ typ = SEMICOLON
  lexeme = ";"
  literal = null
  line = 1 }
{ typ = EOF
  lexeme = ""
  literal = null
  line = 4 }
"""
