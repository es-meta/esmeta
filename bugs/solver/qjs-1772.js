const __res0 = Object.prototype.propertyIsEnumerable.call(null, new Proxy(function(){}, new Proxy({}, { get() { throw new EvalError; } })));
