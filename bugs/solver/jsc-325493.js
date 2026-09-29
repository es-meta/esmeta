const __res0 = Function.prototype.apply.call("a", 2, new Proxy(function(){}, new Proxy({}, { get() { throw new EvalError; } })));
