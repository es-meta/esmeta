const r = Proxy.revocable(function(){}, {});
r.revoke();
Function.prototype.toString.call(r.proxy);
