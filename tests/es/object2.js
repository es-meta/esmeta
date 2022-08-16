var proto = {};

Object.defineProperty(proto, "enumerable", { get: function() { return true; } });
var ConstructFun = function() {};
ConstructFun.prototype = proto;

var descObj = new ConstructFun();
Object.defineProperty(descObj, "enumerable", { value: false });

var newObj = Object.create({}, { prop: descObj });

var f = Object.getOwnPropertyDescriptor(newObj, 'prop');
