namespace LoxTest

open LoxFsharp
open NUnit.Framework

[<TestFixture>]
type TestInterpreter() =

    [<OneTimeSetUp>]
    member this.Setup() = System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

    [<Test>]
    member this.``Test Run File``() =
        let lox = Lox()
        lox.runFile "programs\1.lox"

    [<Test>]
    member this.``Test IF``() =
        let lox = Lox()

        lox.run
            """
var a = 101;
if (a <= 10) {
    print a;
} else if (a > 100) {
    print a / 100;
} else {
    print "no.";
}
"""

    [<Test>]
    member this.``Test Variable In Condition``() =
        let lox = Lox()

        lox.run
            """
var a = 100;
if (true) {
    var b = 100;
} else {
    var b = 10;
}
print a + b;
"""

        Assert.IsTrue lox.hadRuntimeError

        let lox = Lox()

        lox.run
            """
var a = 100;
var b;
if (true) {
    b = 100;
} else {
    b = 10;
}
print a + b;
"""

        Assert.IsFalse lox.hadRuntimeError

    [<Test>]
    member this.``Test While``() =
        let lox = Lox()

        lox.run
            """
var a = 10;
while (a > 0) {
    print a;
    a = a-1;
}
"""

    [<Test>]
    member this.``Test For 1``() =
        let lox = Lox()

        lox.run
            """
var a = 10;
for (var i = 0; i < a; i=i+1) {
    print "===";
    for (;false;) {
        print i;
    }
    print i;
    for (var j = 0; j <= i; j=j+1) {
        print i * j;
    }
}
"""

    [<Test>]
    member this.``Test For 2``() =
        let lox = Lox()

        lox.run
            """
var a = 0;
var temp;

for (var b = 1; a < 1000000; b = temp + b) {
  print a;
  temp = a;
  a = b;
}
"""

    [<Test>]
    member this.``Test Function clock``() =
        let lox = Lox()
        let loopUpper = 5000000
        let loopLower = 0

        lox.run
            """
print "raw loop";
print clock();
"""

        let mutable x = loopUpper

        while x > loopLower do
            x <- x - 1

        lox.run
            """
print clock();
"""

        lox.run
            $"""
print "
lox loop";
print clock();
var a = {loopUpper};
var b = {loopLower};
for (; a>b;a=a-1) a;
print clock();
"""

    [<Test>]
    member this.``Test Function Declaration``() =
        let lox = Lox()

        lox.run
            """
fun add(a, b) {
  return a + b;
}

print add; // "<fn add>".
print add(1,add(2,3));
"""

    [<Test>]
    member this.``Test Fib``() =
        let lox = Lox()

        lox.run
            """
fun fib(n) {
  if (n <= 1) return n;
  return fib(n - 2) + fib(n - 1);
}

for (var i = 0; i < 20; i = i + 1) {
  print fib(i);
}
"""
