var f = function(){};
var x = new f();
var a = f.prototype.isPrototypeOf(x);
var b = Object.prototype.isPrototypeOf(x);
