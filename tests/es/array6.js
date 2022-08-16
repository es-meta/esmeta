Array.prototype.myproperty = 42;
var x = Array();
var y = x.myproperty;
var z = Object.prototype.hasOwnProperty.call(x, 'myproperty');
