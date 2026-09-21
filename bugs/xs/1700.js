const buffer = new ArrayBuffer(8);
const value = new Uint8Array(buffer);
buffer.transfer();
value.setFromHex("a");