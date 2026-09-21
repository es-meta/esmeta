const buffer = new ArrayBuffer(8);
const value = new Float32Array(buffer);
buffer.transfer();
new Float16Array(new ArrayBuffer(8, { maxByteLength: 16 }).transfer(1), 4, value);
