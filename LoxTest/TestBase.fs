namespace LoxTest

open NUnit.Framework

type TestBase() =
    let mutable out = Captin.ConsoleIntercept.ConsoleOut.Observe()

    [<OneTimeSetUp>]
    member this.OneTimeSetUp() = System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

    [<SetUp>]
    member this.SetUp() =
        out.Dispose()
        out <- Captin.ConsoleIntercept.ConsoleOut.Observe()

    member this.CheckOutput(expectStr: string) = Assert.AreEqual(expectStr.Trim(), out.ToString().Trim())
