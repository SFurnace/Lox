module LoxTest.TestInterpreter

open LoxFsharp
open NUnit.Framework

[<OneTimeSetUp>]
let Setup () = System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

[<Test>]
let ``Test Print`` () =
    let lox = Lox()

    lox.run
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

[<Test>]
let ``Test Run File`` () =
    let lox = Lox()
    System.IO.Path.Combine("programs", "1.lox") |> lox.runFile

[<Test>]
let ``Test IF`` () =
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
let ``Test Variable In Condition`` () =
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
let ``Test While`` () =
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
let ``Test For 1`` () =
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
let ``Test For 2`` () =
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
let ``Test Function clock`` () =
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
let ``Test Function Declaration`` () =
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
let ``Test Fib`` () =
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

[<Test>]
let ``Test Make Counter`` () =
    let lox = Lox()

    lox.run
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
let ``Test Func Scope`` () =
    let lox = Lox()

    lox.run
        """
fun scope(a) {
var a = "local";
print a;
}
scope(10);
"""

[<Test>]
let ``Test Scope`` () =
    let lox = Lox()

    lox.run
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
let ``Test Closure`` () =
    let lox = Lox()

    lox.run
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
let ``Test Return Global`` () =
    let lox = Lox()

    lox.run
        """
return 100;
"""
