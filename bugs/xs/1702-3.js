const r = Proxy.revocable(function(){}, {});
r.revoke();
const buffer = new ArrayBuffer(8);
const value = new BigUint64Array(buffer);
buffer.transfer();
const g = (function*(){ yield 0; })();
g.next();
new Set().symmetricDifference({ has: r.proxy, keys: (async function* () {}).constructor(Number.MAX_SAFE_INTEGER), size: true }).intersection(value).union(g).intersection(Promise.reject(0)).add("a");