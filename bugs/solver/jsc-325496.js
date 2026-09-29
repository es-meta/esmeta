Reflect.construct(ArrayBuffer, [-1, -0], new Proxy(Uint8Array, { get(t, p, r) { if (p === "prototype") { return ("a"); } return Reflect.get(t, p, r); } }));
