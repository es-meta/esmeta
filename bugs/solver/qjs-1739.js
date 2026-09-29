Reflect.construct(Int8Array, [...[Number.MAX_SAFE_INTEGER]], (() => { const r = Proxy.revocable(function(){}, {}); r.revoke(); return r.proxy; })());
