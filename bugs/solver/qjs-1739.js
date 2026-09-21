const r = Proxy.revocable(function(){}, {});
r.revoke();
Object.seal(Reflect.construct(Float32Array, [Reflect.construct(ArrayBuffer, [2, Symbol()], Object.defineProperties(function(){}, { prototype: { value: null } }))], r.proxy));
