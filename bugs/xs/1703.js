const buffer = new ArrayBuffer(8);
const value = new Uint32Array(buffer);
buffer.transfer();
value.set(undefined, -Infinity);
