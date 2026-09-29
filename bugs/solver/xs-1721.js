const __res0 = Object.prototype.toString.call(new Proxy(function(){}, new Proxy({}, { get() { throw new EvalError; } })));
