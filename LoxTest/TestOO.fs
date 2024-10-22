namespace LoxTest

open LoxFsharp
open NUnit.Framework

type TestOO() =
    inherit TestBase()


    [<Test>]
    member this.``Test Print Class Name``() =
        Lox().run
            """
class DevonshireCream {
  serveOn() {
    return "Scones";
  }
}

print DevonshireCream;
    """

        this.CheckOutput
            """
<class DevonshireCream>
    """

    [<Test>]
    member this.``Test Print Instance Name``() =
        Lox().run
            """
class Bagel {}
var bagel = Bagel();
print bagel;
    """

        this.CheckOutput
            """
<instance <class Bagel>>
    """


    [<Test>]
    member this.``Test Property``() = ()
