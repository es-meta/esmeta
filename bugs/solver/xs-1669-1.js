const __res0 = Function.prototype.bind.call(new Proxy(BigInt, { get(t, p, r) { if (p === "name") { throw 0; } return Reflect.get(t); } }), "a", ...[]);
