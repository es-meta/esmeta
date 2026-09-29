Reflect.construct(Uint8Array, [...[2 ** 32]], (() => { const r = Proxy.revocable(function(){}, {}); r.revoke(); return r.proxy; })());
