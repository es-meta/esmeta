function* f() { var x = yield; return x; }
var g = f();
var a = g.next().value;
var b = g.next(42).value;
