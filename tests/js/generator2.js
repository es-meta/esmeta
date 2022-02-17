function* f() { let x = 1; while (true) { yield x; x++; } }
var g = f();
var a = g.next().value;
var b = g.next().value;
