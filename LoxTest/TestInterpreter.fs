namespace LoxTest

open LoxFsharp
open NUnit.Framework

type TestInterpreter() =
    inherit TestBase()

    [<Test>]
    member this.``Test Print``() =
        Lox().run
            """
var a;
var b = nil;
print a;
print b;
print a == b;
a = true;
b = true;
print a == b;
print a;
print b = false;
print a == b;
print b;
    """

        this.CheckOutput
            """
nil
nil
true
true
true
false
false
false
    """

    [<Test>]
    member this.``Test Run File``() =
        System.IO.Path.Combine("programs", "1.lox") |> Lox().runFile

        this.CheckOutput
            """
inner a
outer b
global c
outer a
outer b
global c
global a
global b
global c
    """

    [<Test>]
    member this.``Test IF``() =
        Lox().run
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

        this.CheckOutput
            """
1.01
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
        Lox().run
            """
var a = 10;
while (a > 0) {
print a;
a = a-1;
}
    """

    [<Test>]
    member this.``Test For 1``() =
        Lox().run
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
        Lox().run
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
        let loopUpper = 1000000
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
        Lox().run
            """
fun add(a, b) {
return a + b;
}

print add; // "<fn add>".
print add(1,add(2,3));
    """

    [<Test>]
    member this.``Test Fib``() =
        Lox().run
            """
fun fib(n) {
if (n <= 1) return n;
return fib(n - 2) + fib(n - 1);
}

for (var i = 0; i < 20; i = i + 1) {
print fib(i);
}
    """

    [<Test>]
    member this.``Test Make Counter``() =
        Lox().run
            """
fun makeCounter() {
var i = 0;
fun count() {
i = i + 1;
print i;
}

return count;
}

var counter = makeCounter();
counter(); // "1".
counter(); // "2".
    """

    [<Test>]
    member this.``Test Func Scope``() =
        Lox().run
            """
fun scope(a) {
var a = "local";
print a;
}
scope(10);
    """

    [<Test>]
    member this.``Test Scope``() =
        Lox().run
            """
var a = "outer";
{
print a;
var a = "inner";
var a = 100;
print a;
}
print a;
    """

    [<Test>]
    member this.``Test Closure``() =
        Lox().run
            """
var a = "global";
{
fun showA() {
print a;
}

showA();
var a = "block";
showA();
}
    """

    [<Test>]
    member this.``Test Return Global``() =
        Lox().run
            """
return 100;
    """

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
