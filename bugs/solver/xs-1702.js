Function.prototype[Symbol.hasInstance].call((() => { const r = Proxy.revocable(function(){}, {}); r.revoke(); return r.proxy; })(), Promise);
