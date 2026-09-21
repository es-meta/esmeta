const r = Proxy.revocable(function(){}, {});
r.revoke();
Iterator.prototype.every.call({ next: () => ({ get done() { throw 0; } }) }, r.proxy);