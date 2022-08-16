function* f() { yield 1; }
var g = f();
var a = g.next().value;
var b = g.next().value;
