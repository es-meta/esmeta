var o = {};
var s = new Set([NaN, o, "a"]);
var deleted = s.delete(NaN) && s.delete(o);
var hasNaN = s.has(NaN);
var hasObj = s.has(o);
var hasStr = s.has("a");