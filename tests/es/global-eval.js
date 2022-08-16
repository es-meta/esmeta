var a = eval;
a.f = 1;
var b = a.f;
var c = a("var a = eval; a.f = 2; a.f");
