var o = {};
var p = {};
var s = new WeakSet([o, p]);
var deleted = s.delete(o);
var hasO = s.has(o);
var hasP = s.has(p);