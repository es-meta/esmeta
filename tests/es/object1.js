var proto = {};
Object.defineProperty(proto, "enumerable", { get: function() { return true; } });
var ConstructFun = function() {};
ConstructFun.prototype = proto;

var descObj = new ConstructFun();
Object.defineProperty(descObj, "enumerable", { value: false });

var a = proto.enumerable;
var b = descObj.enumerable;
