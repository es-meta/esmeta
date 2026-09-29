const __res0 = Function.prototype.toString.call(new Proxy(function(){}, new Proxy({}, { get() { throw new EvalError; } })));
