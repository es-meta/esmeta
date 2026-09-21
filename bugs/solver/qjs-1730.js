const buffer = new ArrayBuffer(8);
const value = new BigUint64Array(buffer);
buffer.transfer();
Reflect.set(value, 0n, undefined, null);