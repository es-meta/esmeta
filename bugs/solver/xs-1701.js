const buffer = new ArrayBuffer(8);
const value = new Uint8Array(buffer);
buffer.transfer();
"aa".__proto__ = value;