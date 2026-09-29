Function.prototype.toString.call((() => { const r = Proxy.revocable(function(){}, {}); r.revoke(); return r.proxy; })());
