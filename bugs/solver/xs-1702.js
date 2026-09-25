const buffer = new ArrayBuffer(8);
const value = new Uint8ClampedArray(buffer);
buffer.transfer();
const r = Proxy.revocable(function(){}, {});
r.revoke();
Array.from.call(function() { throw 0; }, value, r.proxy, false);