const buffer = new ArrayBuffer(8);
const value = buffer;
buffer.transfer();
new Uint16Array(value, true, Number.MAX_SAFE_INTEGER);
