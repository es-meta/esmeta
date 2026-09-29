Uint8Array.prototype.setFromHex.call((() => { const buffer = new ArrayBuffer(8); const value = new Uint8Array(buffer); buffer.transfer(); return value; })(), "a");
