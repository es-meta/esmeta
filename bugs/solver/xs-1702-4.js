const r = Proxy.revocable(function(){}, {});
r.revoke();
Iterator.prototype.map.call({ get next() { throw 0; } }, r.proxy);
