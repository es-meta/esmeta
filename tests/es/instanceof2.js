function f() {}
var a = f instanceof Function;
var b = f instanceof Object;

var elem = new f;
var c = elem instanceof f;
var d = elem instanceof Object;
