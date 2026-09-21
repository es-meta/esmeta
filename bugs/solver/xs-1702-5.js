const r = Proxy.revocable(function(){}, {});
r.revoke();
Iterator.prototype.reduce.call({ get next() { throw 0; } }, r.proxy);
