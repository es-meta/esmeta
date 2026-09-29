Reflect.construct(Int16Array, [...[-1]], (() => { const r = Proxy.revocable(function(){}, {}); r.revoke(); return r.proxy; })());
